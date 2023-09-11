-module(imetrics_vm_metrics).
-export([install/0, proc_count/2]).

install() ->
    imetrics:set_multigauge(erlang_vm, vm_metric, fun metric_fun/0).

metric_fun() ->
    LastUpdateTime = case ets:lookup(imetrics_vm_metrics, last_update_time) of
        [{last_update_time, T}] -> T;
        [] -> {0,0,0}
    end,

    Table = case (timer:now_diff(erlang:timestamp(), LastUpdateTime) div 1000) > timer:seconds(55) of
        true ->
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

            Objects = [
                {max_message_queue_len, MaxMQueueLen},
                {max_memory, MaxMemory},
                {atom_count, erlang:system_info(atom_count)},
                {atom_limit, erlang:system_info(atom_limit)},
                {process_count, erlang:system_info(process_count)},
                {last_update_time, erlang:timestamp()}
            ],
            ets:insert(imetrics_vm_metrics, Objects),
            Objects;
        false ->
            ets:tab2list(imetrics_vm_metrics)
    end,

    Metrics = lists:keydelete(last_update_time, 1, Table),
    Metrics.

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