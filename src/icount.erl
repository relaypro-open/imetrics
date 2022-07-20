-module(icount).
-behaviour(gen_server).

-export([start_link/1, start_link/2,
         add/3, add/4, put/4, info/1, get/3,
         localize/2, localize/3, delocalize/2,
         dump/1, dump/2, foldl_dump/3, remove/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile({no_auto_import, [get_keys/1]}).

-define(CallTimeout, 20000).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc start a new unnamed gen_server
start_link(Opts=#{}) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc start a new named gen_server
start_link(Ref, Opts=#{}) ->
    gen_server:start_link(Ref, ?MODULE, Opts, []).

%% @doc print operating info about this gen_server (e.g. memory usage)
info(SvrRef) ->
    Ets = get_ets(SvrRef),
    Refs = [ element(2, X) || X <- ets:tab2list(Ets) ],
    Memory = lists:sum([ maps:get(memory, X) || X <- [ counters:info(Y) || Y <- Refs ] ]),
    #{uids => ets:info(Ets, size),
      memory => Memory,
      keys => length(get_keys(SvrRef))}.

%% @doc Localizes a counters object. See localize/3
localize(SvrRef, UId) ->
    localize(SvrRef, UId, 7*timer:hours(24)).

%% @doc Localizes a counters object by doing the following:
%%   1. Artifically updates the last-used timestamp to the future
%%      to help prevent garbage collection. Note: this does not guarantee
%%      that the object will not be garbage collected.
%%   2. Puts the counters object in calling process's local storage.
%%      This allows us to optimize future calls
%%   3. The icount gen_server monitors the calling process so that
%%      when it goes DOWN, icount will automatically call remove/2
%%
%%  localize/3 can be called multiple times to increase the protection
%%  timestamp over time.
%%
%%  Intended use for this feature is to tie the lifetime of a 'counters' object
%%  to a running process (with a unique process identifier for the UId). For
%%  example, in your init/1, you can call localize/2 or localize/3, and 
%%  proceed to increment counters. The icount gen_server will only be accessed
%%  to compute 'new' key indexes on the fly. Everything else is cached in your
%%  process's local storage. When your process ends, the counters object is
%%  cleanup up from icount.
%%
%%  In the meantime, all the counters can still be tracked via the icount
%%  interface from anywhere (e.g. global metrics gathering)
localize(SvrRef, UId, ProtectTime) ->
    LocalizedUIdKey = {?MODULE, counters, UId},
    LocalizedIdxKey = {?MODULE, idx, UId},
    put(LocalizedUIdKey, get_or_create_counters_ref(SvrRef, UId, {protect, ProtectTime})),
    put(LocalizedIdxKey, get_idx(SvrRef)),
    Pid = self(),
    gen_server:cast(SvrRef, {monitor, UId, Pid}),
    ok.

%% @doc Performs the opposite of localize/3
delocalize(SvrRef, UId) ->
    get_or_create_counters_ref(SvrRef, UId, {protect, 0}),
    LocalizedUIdKey = {?MODULE, counters, UId},
    LocalizedIdxKey = {?MODULE, idx, UId},
    erase(LocalizedUIdKey),
    erase(LocalizedIdxKey),
    gen_server:cast(SvrRef, {demonitor, UId, self()}),
    ok.

%% @doc dump all counters to a nested proplist, sorted by recency
dump(SvrRef) ->
    Ets = get_ets(SvrRef),
    Keys = lists:sort(get_keys(SvrRef)),
    Table = ets:tab2list(Ets),
    Sorted = lists:sort(fun({_, _, B}, {_, _, A}) -> A =< B end, Table),
    Now = erlang:monotonic_time(microsecond),
    lists:map(
      fun({UId, CounterRef, Then}) ->
              Diff = (Now - Then) div 1000,
              {UId, [{<<"$ms">>, Diff}] ++ [ {X, get(SvrRef, UId, CounterRef, X)} || X <- Keys ]}
      end, Sorted).

%% @doc fold over all dumped counters (nested proplists), unsorted
foldl_dump(SvrRef, Fun, Acc0) ->
    Ets = get_ets(SvrRef),
    %% Note: cannot sort by last-updated time
    Keys = lists:sort(get_keys(SvrRef)),
    Now = erlang:monotonic_time(microsecond),
    ets:foldl(
      fun({UId, CounterRef, Then}, Acc1) ->
              Diff = (Now - Then) div 1000,
              Data = {UId, [{<<"$ms">>, Diff}] ++ [ {X, get(SvrRef, UId, CounterRef, X)} || X <- Keys ]},
              Fun(Data, Acc1)
      end, Acc0, Ets).

%% @doc dump counters for a specific uid
dump(SvrRef, UId) ->
    Ets = get_ets(SvrRef),
    Keys = lists:sort(get_keys(SvrRef)),
    Now = erlang:monotonic_time(microsecond),
    case ets:lookup(Ets, UId) of
        [{UId, CounterRef, Then}] ->
            Diff = (Now - Then) div 1000,
            [{UId, [{<<"$ms">>, Diff}] ++ [ {X, get(SvrRef, UId, CounterRef, X)} || X <- Keys ]}];
        [] ->
            []
    end.

%% @doc get specific counter value
get(SvrRef, UId, Key) ->
    CountersRef = get_or_create_counters_ref(SvrRef, UId, read),
    get(SvrRef, UId, CountersRef, Key).

%% @doc increment counter value by 1
add(SvrRef, UId, Key) ->
    add(SvrRef, UId, Key, 1).

%% @doc increment counter value by Value
add(SvrRef, UId, Key, Value) ->
    CountersRef = get_or_create_counters_ref(SvrRef, UId, write),
    case key_to_idx(SvrRef, UId, Key) of
        {ok, Idx} ->
            counters:add(CountersRef, Idx, Value);
        Error ->
            Error
    end.

%% @doc put Value to counter value (slower than add)
put(SvrRef, UId, Key, Value) ->
    CountersRef = get_or_create_counters_ref(SvrRef, UId, write),
    case key_to_idx(SvrRef, UId, Key) of
        {ok, Idx} ->
            counters:put(CountersRef, Idx, Value);
        Error ->
            Error
    end.

%% @doc remove 'counters' object entirely. All contained counter
%% values are destroyed. The memory used by this 'counters' object
%% is freed within the Erlang BEAM, and this UId will not appear in
%% future calls to dump and foldl_dump
remove(SvrRef, UId) ->
    gen_server:cast(SvrRef, {remove_counters_ref, UId}).

%% ---------------------------------------------------------------

get(SvrRef, UId, CountersRef, Key) ->
    case key_to_idx(SvrRef, UId, Key) of
        {ok, Idx} ->
            counters:get(CountersRef, Idx);
        {error, idx_overflow} ->
            -1
    end.

%% @doc UId can be any term
get_or_create_counters_ref(SvrRef, UId, Op) ->
    if Op =:= write orelse Op =:= read ->
           LocalizedUIdKey = {?MODULE, counters, UId},
           case get(LocalizedUIdKey) of
               undefined ->
                   gen_server:call(SvrRef, {counters_ref, UId, Op}, ?CallTimeout);
               CountersRef ->
                   CountersRef
           end;
       true ->
           gen_server:call(SvrRef, {counters_ref, UId, Op}, ?CallTimeout)
    end.

get_idx(SvrRef) ->
    gen_server:call(SvrRef, get_idx, ?CallTimeout).

key_to_idx(SvrRef, UId, Key) ->
    LocalizedIdxKey = {?MODULE, idx, UId},
    case get(LocalizedIdxKey) of
        undefined ->
            gen_server:call(SvrRef, {key_to_idx, Key}, ?CallTimeout);
        Idx ->
            case maps:get(Key, Idx, undefined) of
                undefined ->
                    case gen_server:call(SvrRef, {key_to_idx, Key}, ?CallTimeout) of
                        {ok, V} ->
                            Idx2 = Idx#{Key => V},
                            put(LocalizedIdxKey, Idx2),
                            {ok, V};
                        Error ->
                            Error
                    end;
                V ->
                    {ok, V}
            end
    end.

get_keys(SvrRef) ->
    try gen_server:call(SvrRef, get_keys, ?CallTimeout)
    catch exit:{noproc, _} ->
              []
    end.

get_ets(SvrRef) ->
    gen_server:call(SvrRef, get_ets, ?CallTimeout).

init(StartArgs=#{size := Size,
                 opts := Opts,
                 hwm := _Hwm}) ->
    Ets = ets:new(?MODULE, [public]),
    CountersRef = counters:new(Size, Opts),
    Mem = maps:get(memory, counters:info(CountersRef)),
    {ok, StartArgs#{idx => #{},
                    mem => Mem,
                    num => 0,
                    ets => Ets,
                    localized => {#{}, #{}, #{}} % id => count of mrefs currently in use, mref => id, pid => mref
                   }
    }.

handle_call({counters_ref, UId, Op}, _From, State=#{size := Size, opts := Opts, num := N, ets := Ets}) ->
    Now = erlang:monotonic_time(microsecond),
    {Reply, NumCounters} = case ets:lookup(Ets, UId) of
        [{_, CountersRef, _}] ->
            InsertTime = case Op of
                {protect, ProtectTime} ->
                    Now+ProtectTime;
                _ ->
                    Now
            end,
            true = ets:insert(Ets, {UId, CountersRef, InsertTime}),
            {CountersRef, N};
        [] ->
            gen_server:cast(self(), gc),
            CountersRef = counters:new(Size, Opts),
            case Op of
                write ->
                   true = ets:insert(Ets, {UId, CountersRef, Now});
                {protect, ProtectTime} ->
                   true = ets:insert(Ets, {UId, CountersRef, Now+ProtectTime});
                _ ->
                   ok
            end,
            {CountersRef, N+1}
    end,
    {reply, Reply, State#{num => NumCounters}};
handle_call({key_to_idx, Key}, _From, State=#{idx := Idx, size := Size}) ->
    {Reply, Idx2} = case maps:get(Key, Idx, undefined) of
        undefined ->
            V = 1+maps:size(Idx),
            if V > Size ->
                   {{error, idx_overflow}, Idx};
               true ->
                   {{ok, V}, Idx#{Key => V}}
            end;
        V ->
            {{ok, V}, Idx}
    end,
    {reply, Reply, State#{idx => Idx2}};
handle_call(get_keys, _From, State=#{idx := Idx}) ->
    {reply, maps:keys(Idx), State};
handle_call(get_ets, _From, State=#{ets := Ets}) ->
    {reply, Ets, State};
handle_call(get_idx, _From, State=#{idx := Idx}) ->
    {reply, Idx, State}.

handle_cast({monitor, UId, Pid}, State=#{localized := {IMap, RMap, PidMrefMap}}) ->
    MRef = erlang:monitor(process, Pid),
    MRefCount = maps:get(UId, IMap, 0),
    IMap2 = IMap#{UId => MRefCount+1},     %% increment the ref counter for this id
    RMap2 = RMap#{MRef => UId},
    PidMrefMap2 = PidMrefMap#{Pid => MRef},
    {noreply, State#{localized => {IMap2, RMap2, PidMrefMap2}}};
handle_cast({demonitor, UId, Pid}, State=#{localized := {_IMap, _RMap, PidMrefMap}}) ->
    case maps:get(Pid, PidMrefMap, undefined) of
        undefined ->
            {noreply, State};
        MRef ->
            erlang:demonitor(MRef),
            {noreply, remove_process(UId, MRef, Pid, State)}
    end;
handle_cast({remove_counters_ref, UId}, State=#{num := N, ets := Ets}) ->
    N2 = case ets:lookup(Ets, UId) of
        [{_, _CountersRef, _}] ->
            ets:delete(Ets, UId),
            N-1;
        [] ->
            N
    end,
    {noreply, State#{num => N2}};
handle_cast(gc, State=#{mem := Mem,
                        num := Num,
                        hwm := Hwm,
                        ets := Ets}) ->
    TotalMem = Mem*Num,
    if TotalMem > Hwm ->
           %% choose the number of counters to delete
           %%   A. Make sure the number we choose will put us below the hwm [Expire]
           %%   B. Make sure we delete at least 10% [max(Num div 10, A)]
           %%   C. Don't try to delete more than we have [min(Num, B)]
           Expire = round((TotalMem - Hwm) / Mem),
           Expire2 = erlang:min(Num, erlang:max(Num div 10, Expire)),

           {_, _, Cutoff} = quickselect:select(fun({_, _, A}, {_, _, B}) -> A =< B end, Expire2, ets:tab2list(Ets)),
           Deleted = ets:select_delete(Ets, [{{'_', '_', '$1'}, [{'<', '$1', Cutoff}], [true]}]),

           {noreply, State#{num => Num - Deleted}};
       true ->
           {noreply, State}
    end.

handle_info({sleep, N}, State) ->
    do_sleep(N),
    {noreply, State};
handle_info({'DOWN', MRef, process, DownPid, _Info}, State=#{localized := {_IMap, RMap, _PidMrefMap}}) ->
    case maps:get(MRef, RMap, undefined) of
        undefined ->
            {noreply, State};
        UId ->
            {noreply, remove_process(UId, MRef, DownPid, State)}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% untracks the provided mref/pid and decrements number of tracked processes, cleaning up counters if zero
remove_process(UId, MRef, Pid, State=#{localized := {IMap, RMap, PidMrefMap}}) ->
    NMRefCount = maps:get(UId, IMap, 0) - 1,
    IMap2 = case NMRefCount of
        C when C =< 0 ->
            %% shut it down
            remove(self(), UId), % gen_server:cast
            maps:without([UId], IMap);
        C ->
            IMap#{UId => C}
    end,
    RMap2 = maps:without([MRef], RMap),
    PidMrefMap2 = maps:without([Pid], PidMrefMap),
    State#{localized => {IMap2, RMap2, PidMrefMap2}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extras) ->
    {ok, State}.

%% In order to test the localization of a counters object,
%% this sleep function is provided. It will keep the gen_server
%% in a busy state for N milliseconds, during which an eunit test
%% can operate on the localized counters object. If we're not in an
%% eunit test, this function is a no-op
-ifdef(EUNIT).
do_sleep(N) ->
    ?debugFmt("sleeping ~p within an eunit test", [N]),
    timer:sleep(N).
-else.
do_sleep(_) -> ok.
-endif.
