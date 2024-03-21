-module(imetrics_vm_metrics).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([install/0, proc_count/2]).
-define(RefreshInterval, 60000). % 60 seconds

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    State = #{
        installed => false
    },
    {ok, State}.

handle_info(refresh_gauges, State) ->
    [
        [{MaxMQueuePid, MaxMQueueLen, MaxMQueueProps}|_],
        [{MaxMemoryPid, MaxMemory, MaxMemoryProps}|_]
    ] = proc_count([
        message_queue_len,
        memory
    ], 1),

    % if the memory is > 512MB, log the PID to console
    case MaxMemory > 512000000 of
        true ->
            MaxMemoryCurrentFunc = proplists:get_value(current_function, MaxMemoryProps, unknown_current_function),
            MaxMemoryInitialCall = proplists:get_value(initial_call, MaxMemoryProps, unknown_initial_call),
            MaxMemoryRegisteredName = proplists:get_value(registered_name, MaxMemoryProps, unknown_registered_name),
            logger:info("Max Memory PID: ~p, current_function: ~w, registered_name: ~w, initial_call: ~w", [
                MaxMemoryPid,
                MaxMemoryCurrentFunc,
                MaxMemoryRegisteredName,
                MaxMemoryInitialCall
            ]);
        false ->
            noop
    end,

    % if a process has more than 50 messages, log the PID
    case MaxMQueueLen > 50 of
        true ->
            MaxMQueueCurrentFunc = proplists:get_value(current_function, MaxMQueueProps, unknown_current_function),
            MaxMQueueInitialCall = proplists:get_value(initial_call, MaxMQueueProps, unknown_initial_call),
            MaxMQueueRegisteredName = proplists:get_value(registered_name, MaxMQueueProps, unknown_registered_name),
            logger:info("Max Message Queue PID: ~p, current_function: ~w, registered_name: ~w, initial_call: ~w", [
                MaxMQueuePid,
                MaxMQueueCurrentFunc,
                MaxMQueueRegisteredName,
                MaxMQueueInitialCall
            ]);
        false ->
            noop
    end,

    AtomCount = erlang:system_info(atom_count),
    AtomLimit = erlang:system_info(atom_limit),
    ProcessCount = erlang:system_info(process_count),

    imetrics:set_gauge(erlang_vm, #{ vm_metric => max_message_queue_len }, MaxMQueueLen),
    imetrics:set_gauge(erlang_vm, #{ vm_metric => max_memory }, MaxMemory),
    imetrics:set_gauge(erlang_vm, #{ vm_metric => atom_count }, AtomCount),
    imetrics:set_gauge(erlang_vm, #{ vm_metric => atom_limit }, AtomLimit),
    imetrics:set_gauge(erlang_vm, #{ vm_metric => process_count }, ProcessCount),
    imetrics:set_gauge(erlang_vm, #{ vm_metric => last_update_time }, os:system_time(second)),

    % recalculate after the interval time has passed
    erlang:send_after(?RefreshInterval, self(), refresh_gauges),

    % We don't need to reply nor update the state.
    {noreply, State};
% for any other infos, fail
handle_info(_Info, State) ->
    {noreply, State}.

% for any other casts, fail.
handle_cast(_Cast, _State) ->
    erlang:error(function_clause).

% only allow the server to be "installed" once
handle_call(install, _From, #{ installed := false }) ->
    self() ! refresh_gauges,
    {reply, ok, #{ installed => true }};
handle_call(install, _From, State) ->
    {reply, already_installed, State};
handle_call(_Call, _From, _State) ->
    erlang:error(function_clause).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extras) ->
    {ok, State}.

% designed to be called only once
install() ->
    gen_server:call(?MODULE, install).

% A modification of recon:proc_count that allows you to fetch multiple
% attributes without calling process_info multiple times.
proc_count(AttrList, Num) ->
    ProcInfos = proc_attrs(AttrList),
    ProcInfosByAttr = lists:foldl(fun (ProcInfo, ProcInfosByAttr) ->
        {Pid, Results, Info} = ProcInfo,
        [[{Pid, AttrValue, Info}|AttrResultsList] || {AttrResultsList, {_AttrName, AttrValue}} <- lists:zip(ProcInfosByAttr, Results)]
    end, [[] || _ <- AttrList], ProcInfos),
    [recon_lib:sublist_top_n_attrs(AttrResultsList, Num) || AttrResultsList <- ProcInfosByAttr].

% modified from recon
% https://github.com/ferd/recon/blob/9efec263d84d0435230f4d3312a4afb352b8f71c/src/recon_lib.erl#L78
proc_attrs(AttrList) ->
    Self = self(),
    FullAttrList = [registered_name, current_function, initial_call] ++ AttrList,
    [Attrs || Pid <- processes(),
              Pid =/= Self,
              {ok, Attrs} <- [proc_attrs(FullAttrList, Pid)]
    ].

% https://github.com/ferd/recon/blob/9efec263d84d0435230f4d3312a4afb352b8f71c/src/recon_lib.erl#L100
proc_attrs(AttrList, Pid) ->
    case process_info(Pid, AttrList) of
        [{registered_name,Name}, Init, Cur|Results] ->
            {ok, {Pid, Results, [{registered_name, Name} || is_atom(Name)]++[Init, Cur]}};
        undefined ->
            {error, undefined}
    end.