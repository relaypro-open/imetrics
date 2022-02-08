-module(imetrics_tests).
-include_lib("eunit/include/eunit.hrl").

start() ->
    application:load(imetrics),
    application:set_env(imetrics, http_server_port, 8086),
    application:ensure_all_started(imetrics),
    #{}.

stop(_Fixture) ->
    application:stop(imetrics).

%% add tests
add_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun add_test/1}. 

add_test(_Fixture) ->
    [ imetrics:add(many_incr) || _ <- lists:seq(1, 4) ],
    [
        ?_assertEqual(1, imetrics:add(atom)),
        ?_assertEqual(5, imetrics:add(atom_set, 5)),
        ?_assertEqual(1, imetrics:add(<<"binary">>)),
        ?_assertEqual(5, imetrics:add(<<"binary_set">>, 5)),
        ?_assertEqual(1, imetrics:add("list")),
        ?_assertEqual(5, imetrics:add("list_set", 5)),
        ?_assertEqual(1, imetrics:add_m(atom_m, atom_k)),
        ?_assertEqual(5, imetrics:add_m(atom_m, atom_k_set, 5)),
        ?_assertEqual(1, imetrics:add_m(<<"binary_m">>, <<"binary_k">>)),
        ?_assertEqual(5, imetrics:add_m(<<"binary_m">>, <<"binary_k_set">>, 5)),
        ?_assertEqual(1, imetrics:add_m("list_m", "list_k")),
        ?_assertEqual(5, imetrics:add_m("list_m", "list_k_set", 5)),
        ?_assertEqual(1, imetrics:add_m(atom_m, 200)),
        ?_assertEqual(5, imetrics:add(many_incr)),
        ?_assertEqual(1, imetrics:add({a,b,c})),
        ?_assertEqual(1, imetrics:add(200)),
        ?_assertEqual({error,{function_clause,check_inputs}}, imetrics:add({a,b,c,d,e,f,g,h,i})),
        ?_assertEqual({error,{badmatch,check_inputs}}, imetrics:add(bad, map))
    ].

gauge_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun gauge_test/1}.

gauge_test(_Fixture) ->
    [ imetrics:set_gauge(many_set, X) || X <- lists:seq(1, 4) ],
    F = 1.5,
    [
        ?_assertEqual(F, imetrics:set_gauge(atom, F)),
        ?_assertEqual(F, imetrics:set_gauge_m(atom_m, atom_k, F)),
        ?_assertEqual(F, imetrics:set_gauge(many_set, F)),
        ?_assertEqual(1, imetrics:update_gauge(updated, 1)),
        ?_assertEqual(0, imetrics:update_gauge(updated, -1)),
        ?_assertEqual(1, imetrics:update_gauge_m(updated, a, 1)),
        ?_assertEqual(-1, imetrics:update_gauge_m(updated, a, -2))
    ].

multigauge_test_() ->
    {setup, fun start/0, fun stop/1, fun multigauge_test/1}.

multigauge_test(_) ->
    imetrics:set_multigauge(multigauge_test, dim, fun() -> [{<<"test">>, 3}] end),
    Gauges = imetrics:get_gauges(),
    [
     ?_assertEqual([{<<"$dim">>,<<"dim">>},{<<"test">>,3}], proplists:get_value(<<"multigauge_test">>, Gauges))
    ].

dimension_test_() ->
    {setup, fun start/0, fun stop/1, fun dimension_test/1}.

dimension_test(_) ->
    imetrics:set_counter_dimension(dimension_test1, dim1),
    imetrics:add_m(dimension_test2, key2),
    imetrics:set_counter_dimension(dimension_test2, dim2),
    [
     ?_assertEqual([{<<"dimension_test1">>,[{<<"$dim">>,<<"dim1">>}]},
                  {<<"dimension_test2">>,
                   [{<<"$dim">>,<<"dim2">>},{<<"key2">>,1}]}], imetrics:get_counters())
    ].

%% badarg tests
badarg_test_() ->
    {setup,
        fun() -> ok end,
        fun(_) -> ok end,
        fun badarg_test/1}.

badarg_test(_Fixture) ->
    % Make sure we don't crash if ets tables do not exist
    [
        ?_assertEqual({error,{badarg,check_ets}}, imetrics:add(atom)),
        ?_assertEqual({error,{badarg,check_ets}}, imetrics:add_m(atom_m, k)),
        ?_assertEqual({error,{badarg,check_ets}}, imetrics:set_gauge(atom_g, 1.5)),
        ?_assertEqual({error,{badarg,check_ets}}, imetrics:set_gauge_m(atom_gm, k, 1.5))
    ].


%% get tests
get_test_() ->
    {setup,
        fun start_get/0,
        fun stop/1,
        fun get_test/1}. 

start_get() ->
    F = start(),
    imetrics:add(counter),
    imetrics:add_m(mapped_counter, key),
    imetrics:set_gauge(gauge, 1.5),
    imetrics:set_gauge_m(mapped_gauge, key, 1.5),
    imetrics:set_gauge(fn_gauge, fun() -> 1.5 end),
    imetrics:add({counter, tuple}),
    application:set_env(imetrics, separator, <<":">>),
    imetrics:add({counter, tuple, colon}),
    application:unset_env(imetrics, separator),
    F.

get_test(_Fixture) ->
    Data = imetrics:get(),
    [
        ?_assertEqual(1, proplists:get_value(<<"counter">>, Data)),
        ?_assertEqual([{<<"key">>, 1}],
            proplists:get_value(<<"mapped_counter">>, Data)),
        ?_assertEqual(1.5, proplists:get_value(<<"gauge">>, Data)),
        ?_assertEqual([{<<"key">>, 1.5}],
            proplists:get_value(<<"mapped_gauge">>, Data)),
        ?_assertEqual(1.5, proplists:get_value(<<"fn_gauge">>, Data))
    ].

%% http tests
http_test_() ->
    {setup,
        fun start_http/0,
        fun stop/1,
        fun http_test/1}. 

start_http() ->
    F = start_get(),
    ok = imetrics_http_server:await(1000),
    {ok, Port} = imetrics_http_server:port(),
    F#{port => Port}.

http_test(#{port := Port}) ->
    Url = "http://localhost:"++integer_to_list(Port)++"/imetrics/varz:get",
    {ok, {Status, _Headers, Body}} = 
        httpc:request(get, {Url, []}, [], []),
    {_Vsn, Code, _Friendly} = Status,

    Res = ["counter 1",
        "counter_tuple 1",
        "counter:tuple:colon 1",
        "fn_gauge 1.5",
        "gauge 1.5",
        "mapped_counter key:1",
        "mapped_gauge key:1.5"],
    Res2 = string:join(Res, "\n") ++ "\n",
    [
        ?_assertEqual(200, Code),
        ?_assertEqual(Res2, Body)
    ].

%% tick/tock tests
ticktock_test_() ->
    {setup,
        fun start_ticktock/0,
        fun stop/1,
        fun ticktock_test/1}. 

start_ticktock() ->
    F = start(),
    imetrics:hist(test, [0, 1000], 50),
    imetrics:hist(test2, [0, 1000], 50),
    F.

ticktock_test(_Fixture) ->
    [
        ?_assertEqual({15, 1}, (fun() ->
                    Tick = imetrics:tick(test, millisecond),
                    timer:sleep(285),
                    imetrics:tock(Tick)
            end)()),
        ?_assertEqual({15, 1}, (fun() ->
                    Tick = imetrics:tick(test, millisecond),
                    timer:sleep(285),
                    imetrics:tock_as(Tick, test2)
            end)())
    ].

ticktock_s_test_() ->
    {setup,
        fun start_ticktock/0,
        fun stop/1,
        fun ticktock_s_test/1}. 

ticktock_s_test(_Fixture) ->
    [
     ?_assertMatch({15, 1}, (fun() ->
                                     {Ref, Ticks} = imetrics:tick_s(#{}, test, millisecond),
                                     timer:sleep(285),
                                     {Result, Ticks2} = imetrics:tock_s(Ticks, Ref),
                                     0 = map_size(Ticks2),
                                     Result
                             end)()),
     ?_assertMatch({15, 2}, (fun() ->
                                     {_Ref, Ticks} = imetrics:tick_s(#{}, test, millisecond),
                                     timer:sleep(285),
                                     {Result, Ticks2} = imetrics:tock_s(Ticks, test),
                                     0 = map_size(Ticks2),
                                     Result
                             end)())
    ].
