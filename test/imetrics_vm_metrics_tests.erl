-module(imetrics_vm_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

start() ->
    application:load(imetrics),
    application:set_env(imetrics, http_server_port, 8087),
    application:ensure_all_started(imetrics),
    #{}.

stop(_Fixture) ->
    application:stop(imetrics).

%% http tests
http_test_() ->
    {setup,
        fun start_http/0,
        fun stop/1,
        fun http_test/1}. 

start_http() ->
    F = start(),
    ok = imetrics_http_server:await(1000),
    {ok, Port} = imetrics_http_server:port(),
    F#{port => Port}.

http_test(#{port := Port}) ->
    http_test_blank(#{port => Port}) ++
    http_test_vm_metrics(#{port => Port}).

http_test_blank(#{port := Port}) ->
    Url = "http://localhost:"++integer_to_list(Port)++"/metrics",
    {ok, {Status, _Headers, Body}} = 
        httpc:request(get, {Url, []}, [], []),
    {_Vsn, Code, _Friendly} = Status,

    Res = [
        "# EOF"
    ],
    Res2 = string:join(Res, "\n") ++ "\n",
    [
        ?_assertEqual(Res2, Body),
        ?_assertEqual(200, Code)
    ].

http_test_vm_metrics(#{port := Port}) ->
    % reduce to 5 seconds from the default of 60s
    application:set_env(imetrics, vm_metrics_refresh_interval, 5_000),
    imetrics_vm_metrics:install(),

    timer:sleep(1_000), % sleep for 1 second to allow the vm_metrics to populate

    Url = "http://localhost:"++integer_to_list(Port)++"/metrics",
    {ok, {Status1, _Headers1, Body1}} = 
        httpc:request(get, {Url, []}, [], []),
    {_Vsn1, Code1, _Friendly1} = Status1,

    timer:sleep(6_000), % sleep for 6 seconds for the metrics to update

    {ok, {Status2, _Headers2, Body2}} = 
        httpc:request(get, {Url, []}, [], []),
    {_Vsn2, Code2, _Friendly2} = Status2,

    timer:sleep(1_000), % sleep for 1 second, we don't expect the metrics to update

    {ok, {Status3, _Headers3, Body3}} = 
        httpc:request(get, {Url, []}, [], []),
    {_Vsn3, Code3, _Friendly3} = Status3,

    [
        ?_assertNotMatch(nomatch, string:find(Body1, "vm_metric=\"atom_limit\"")),
        ?_assertNotMatch(nomatch, string:find(Body1, "vm_metric=\"max_memory\"")),
        ?_assertNotMatch(nomatch, string:find(Body1, "vm_metric=\"max_message_queue_len\"")),
        ?_assertNotMatch(nomatch, string:find(Body1, "vm_metric=\"atom_count\"")),
        ?_assertNotMatch(nomatch, string:find(Body1, "vm_metric=\"process_count\"")),
        ?_assertNotMatch(nomatch, string:find(Body1, "vm_metric=\"last_update_time\"")),
        ?_assertNotEqual(Body1, Body2),
        ?_assertNotMatch(nomatch, string:find(Body2, "vm_metric=\"atom_limit\"")),
        ?_assertNotMatch(nomatch, string:find(Body2, "vm_metric=\"max_memory\"")),
        ?_assertNotMatch(nomatch, string:find(Body2, "vm_metric=\"max_message_queue_len\"")),
        ?_assertNotMatch(nomatch, string:find(Body2, "vm_metric=\"atom_count\"")),
        ?_assertNotMatch(nomatch, string:find(Body2, "vm_metric=\"process_count\"")),
        ?_assertNotMatch(nomatch, string:find(Body2, "vm_metric=\"last_update_time\"")),
        ?_assertEqual(Body2, Body3),
        ?_assertNotMatch(nomatch, string:find(Body3, "vm_metric=\"atom_limit\"")),
        ?_assertNotMatch(nomatch, string:find(Body3, "vm_metric=\"max_memory\"")),
        ?_assertNotMatch(nomatch, string:find(Body3, "vm_metric=\"max_message_queue_len\"")),
        ?_assertNotMatch(nomatch, string:find(Body3, "vm_metric=\"atom_count\"")),
        ?_assertNotMatch(nomatch, string:find(Body3, "vm_metric=\"process_count\"")),
        ?_assertNotMatch(nomatch, string:find(Body3, "vm_metric=\"last_update_time\"")),
        ?_assertEqual(200, Code1),
        ?_assertEqual(200, Code2),
        ?_assertEqual(200, Code3)
    ].