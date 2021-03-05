-module(icount).
-behaviour(gen_server).

-export([start_link/1, start_link/2,
         add/3, add/4, put/4, info/1, get/3, dump/1, dump/2, foldl_dump/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile({no_auto_import, [get_keys/1]}).

-define(CallTimeout, 20000).

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
    case key_to_idx(SvrRef, Key) of
        {ok, Idx} ->
            counters:add(CountersRef, Idx, Value);
        Error ->
            Error
    end.

%% @doc put Value to counter value (slower than add)
put(SvrRef, UId, Key, Value) ->
    CountersRef = get_or_create_counters_ref(SvrRef, UId, write),
    case key_to_idx(SvrRef, Key) of
        {ok, Idx} ->
            counters:put(CountersRef, Idx, Value);
        Error ->
            Error
    end.

%% ---------------------------------------------------------------

get(SvrRef, _UId, CountersRef, Key) ->
    case key_to_idx(SvrRef, Key) of
        {ok, Idx} ->
            counters:get(CountersRef, Idx);
        {error, idx_overflow} ->
            -1
    end.

%% @doc UId can be any term
get_or_create_counters_ref(SvrRef, UId, Op) ->
    gen_server:call(SvrRef, {counters_ref, UId, Op}, ?CallTimeout).

key_to_idx(SvrRef, Key) ->
    gen_server:call(SvrRef, {key_to_idx, Key}, ?CallTimeout).

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
           ets => Ets}}.

handle_call({counters_ref, UId, Op}, _From, State=#{size := Size, opts := Opts, num := N, ets := Ets}) ->
    Now = erlang:monotonic_time(microsecond),
    {Reply, NumCounters} = case ets:lookup(Ets, UId) of
        [{_, CountersRef, _}] ->
            true = ets:insert(Ets, {UId, CountersRef, Now}),
            {CountersRef, N};
        [] ->
            gen_server:cast(self(), gc),
            CountersRef = counters:new(Size, Opts),
            if Op =:= write ->
                   true = ets:insert(Ets, {UId, CountersRef, Now});
               true ->
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
    {reply, Ets, State}.

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

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extras) ->
    {ok, State}.
