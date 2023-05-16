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
                [{MaxMQueuePid, MaxMQueueLen, _}|_],
                [{MaxMemoryPid, MaxMemory, _}|_]
            ] = proc_count([
                message_queue_len,
                memory
            ], 1),
            Objects = [
                {max_message_queue_len, MaxMQueueLen},
                {max_memory, MaxMemory},
                {atom_count, erlang:system_info(atom_count)},
                {atom_limit, erlang:system_info(atom_limit)},
                {last_update_time, erlang:timestamp()}
            ],
            logger:info("Max Message Queue PID: ~p, Max Memory PID: ~p", [MaxMQueuePid, MaxMemoryPid]),
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
            {ok, {Pid, Results, [Name || is_atom(Name)]++[Init, Cur]}};
        undefined ->
            {error, undefined}
    end.