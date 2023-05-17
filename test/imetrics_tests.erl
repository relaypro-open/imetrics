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
     ?_assertEqual([{<<"dimension_test2">>,
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

%%exemplar tests
exemplar_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun exemplar_test/1}. 

exemplar_test(_Fixture) ->
    ?_assertEqual(true, imetrics:set_exemplar(a, 1)),
    ?_assertEqual(true, imetrics:set_exemplar(b, #{c => 1}, 2)),
    ?_assertEqual(true, imetrics:set_exemplar(c, 2, #{d => 2, e => "aa"})),
    ?_assertEqual(true, imetrics:set_exemplar(f, 3, 946684800)),
    ?_assertEqual(true, imetrics:set_exemplar(g, #{h => 1, i=> 2}, 3, #{j => k})),
    ?_assertEqual(true, imetrics:set_exemplar(g, #{h => 2}, 4, 978307200)),
    ?_assertEqual(true, imetrics:set_exemplar(i, 5, #{j => "bb"}, 1009843200)),
    ?_assertEqual(true, imetrics:set_exemplar(k, #{l => 3}, 6, #{m => "cc"}, 1577836800)),
    

    imetrics:set_exemplar(a, 1),
    imetrics:set_exemplar(b, #{c => 1}, 2),
    imetrics:set_exemplar(c, 2, #{d => 2, e => "aa"}),
    imetrics:set_exemplar(f, 3, 946684800),
    imetrics:set_exemplar(g, #{h => 1, i=> 2}, 3, #{j => k}),
    imetrics:set_exemplar(g, #{h => 2}, 4, 978307200),
    imetrics:set_exemplar(i, 5, #{j => "bb"}, 1009843200),
    imetrics:set_exemplar(k, #{l => 3}, 6, #{m => "cc"}, 1577836800),

    ?_assertMatch({1, #{}, _}, imetrics:get_exemplar(#{name => <<"a">>})),
    ?_assertMatch({2, #{}, _}, imetrics:get_exemplar(#{name => <<"b">>, c => <<"1">>})),
    Test3Map = #{d => <<"2">>, e => <<"aa">>},
    ?_assertMatch({2, Test3Map, _}, imetrics:get_exemplar(#{name => <<"c">>})),
    ?_assertMatch({3, #{}, 946684800}, imetrics:get_exemplar(#{name => <<"f">>})),
    Test5Map = #{j => <<"k">>},
    ?_assertMatch({3, Test5Map, _}, imetrics:get_exemplar(#{name => <<"g">>, h => <<"1">>, i => <<"2">>})),
    ?_assertMatch({4, #{}, 978307200}, imetrics:get_exemplar(#{name => <<"g">>, h => <<"2">>})),
    Test7Map = #{j => <<"bb">>},
    ?_assertMatch({5, Test7Map, 1009843200}, imetrics:get_exemplar(#{name => <<"i">>})),
    Test8Map = #{m => <<"cc">>},
    ?_assertMatch({6, Test8Map, 1577836800}, imetrics:get_exemplar(#{name => <<"k">>, l => <<"3">>})),
    
    ?_assertEqual(true, imetrics:set_exemplar(k, #{l => 3}, 0.1, 1893456000)),
    imetrics:set_exemplar(k, #{l => 3}, 0.1, 1893456000),
    ?_assertMatch({0.1, #{}, 1893456000}, imetrics:get_exemplar(#{name => <<"k">>, l => <<"3">>})).


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
    imetrics:add(tagged_counter, #{ tag1 => "val1" }),
    imetrics:set_gauge(gauge, 1.5),
    imetrics:set_gauge_m(mapped_gauge, value, 1.5),
    imetrics:set_counter_dimension(mapped_gauge, key),
    imetrics:set_gauge(fn_gauge, fun() -> 1.5 end),
    imetrics:add({counter, tuple}),
    application:set_env(imetrics, separator, <<":">>),
    imetrics:add({counter, tuple, colon}),
    application:unset_env(imetrics, separator),
    Stats = imetrics:stats(stats),
    Stats2 = imetrics_stats:update(10, Stats),
    imetrics:set_stats(stats, Stats2),

    imetrics:add(counter2),
    imetrics:add(tagged_counter2, #{tag2 => "val1"}),
    imetrics:set_gauge(gauge2, 0.6),
    imetrics:set_exemplar(counter2, 3, #{testLabel => testLabelValue}, 1262304000),
    imetrics:set_exemplar(tagged_counter2, #{tag2 => "val1"}, 1.23, 946684800),
    imetrics:set_exemplar(tagged_counter2, 100, 946684800),
    imetrics:set_exemplar(gauge2, 0.57, 946684800),
    F.

get_test(_Fixture) ->
    Data = imetrics:get(),
    [
        ?_assertEqual(1, proplists:get_value(<<"counter">>, Data)),
        ?_assertEqual([{<<"key">>, 1}],
            proplists:get_value(<<"mapped_counter">>, Data)),
        ?_assertEqual(1.5, proplists:get_value(<<"gauge">>, Data)),
        ?_assertEqual([{<<"value">>, 1.5}],
            proplists:get_value(<<"mapped_gauge">>, Data)),
        ?_assertEqual(1.5, proplists:get_value(<<"fn_gauge">>, Data)),
        ?_assertEqual(#{<<"max">> => 10, <<"min">> => 10, <<"n">> => 1, <<"sum">> => 10,
            <<"sum2">> => 100}, proplists:get_value(<<"stats">>, Data))
    ].

%% get_with_types tests
get_with_types_test_() ->
    {setup,
        fun start_get_with_types/0,
        fun stop/1,
        fun get_with_types_test/1}. 

start_get_with_types() ->
    F = start(),
    imetrics:add(counter, #{ map => key }),
    imetrics:add_m(mapped_counter, value),
    imetrics:set_counter_dimension(mapped_counter, key),
    imetrics:set_gauge(gauge, #{ map2 => key2 }, 1.5),
    imetrics:set_gauge_m(mapped_gauge, value, 1.5),
    imetrics:set_counter_dimension(mapped_gauge, key),
    imetrics:set_multigauge(fn_gauge, code, fun() -> [{<<"200">>, 1}, {<<"404">>, 5}] end),
    F.

get_with_types_test(_Fixture) ->
    Data = imetrics:get_with_types(),
    [
        ?_assertEqual({counter, [{#{ map => <<"key">> }, 1}]}, proplists:get_value(<<"counter">>, Data)),
        ?_assertEqual({counter, [{#{key => <<"value">>}, 1}]},
            proplists:get_value(<<"mapped_counter">>, Data)),
        ?_assertEqual({gauge, [{#{ map2 => <<"key2">>}, 1.5}]}, proplists:get_value(<<"gauge">>, Data)),
        ?_assertEqual({gauge, [{#{key => <<"value">>}, 1.5}]},
            proplists:get_value(<<"mapped_gauge">>, Data)),
        ?_assertEqual({gauge, [{#{ code => <<"200">>}, 1},{#{ code => <<"404">>}, 5}]}, proplists:get_value(<<"fn_gauge">>, Data))
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
    http_test_varz(#{port => Port}) ++ http_test_openmetrics(#{port => Port}) ++
    http_test_varz_strict(#{port => Port}) ++ http_test_openmetrics_strict(#{port => Port}).

http_test_varz(#{port := Port}) ->
    Url = "http://localhost:"++integer_to_list(Port)++"/imetrics/varz:get",
    {ok, {Status, _Headers, Body}} = 
        httpc:request(get, {Url, []}, [], []),
    {_Vsn, Code, _Friendly} = Status,

    Res = ["counter 1",
        "counter2 1",
        "counter:tuple:colon 1",
        "counter_tuple 1",
        "mapped_counter key:1",
        "fn_gauge 1.5",
        "gauge 1.5",
        "gauge2 0.6",
        "mapped_gauge $dim:key value:1.5",
        "stats max:10 min:10 n:1 sum:10 sum2:100"],
    Res2 = string:join(Res, "\n") ++ "\n",
    [
        ?_assertEqual(200, Code),
        ?_assertEqual(Res2, Body)
    ].

http_test_openmetrics(#{port := Port}) ->
    Url = "http://localhost:"++integer_to_list(Port)++"/metrics",
    {ok, {Status, _Headers, Body}} = 
        httpc:request(get, {Url, []}, [], []),
    {_Vsn, Code, _Friendly} = Status,

    Res = [
        "# TYPE counter counter",
        "counter{} 1",
        "# TYPE counter2 counter",
        "counter2{} 1 # {testLabel=\"testLabelValue\"} 3 1262304000",
        "# TYPE counter:tuple:colon counter",
        "counter:tuple:colon{} 1",
        "# TYPE counter_tuple counter",
        "counter_tuple{} 1",
        "# TYPE mapped_counter counter",
        "mapped_counter{map_key=\"key\"} 1",
        "# TYPE tagged_counter counter",
        "tagged_counter{tag1=\"val1\"} 1",
        "# TYPE tagged_counter2 counter",
        "tagged_counter2{tag2=\"val1\"} 1 # {} 1.23 946684800",
        "# TYPE fn_gauge gauge",
        "fn_gauge{} 1.5",
        "# TYPE gauge gauge",
        "gauge{} 1.5",
        "# TYPE gauge2 gauge",
        "gauge2{} 0.6 # {} 0.57 946684800",
        "# TYPE mapped_gauge gauge",
        "mapped_gauge{key=\"value\"} 1.5",
        "# TYPE stats unknown",
        "stats{map_key=\"max\"} 10",
        "stats{map_key=\"min\"} 10",
        "stats{map_key=\"n\"} 1",
        "stats{map_key=\"sum\"} 10",
        "stats{map_key=\"sum2\"} 100",
        "# EOF"],
    Res2 = string:join(Res, "\n") ++ "\n",
    [
        ?_assertEqual(Res2, Body),
        ?_assertEqual(200, Code)
    ].

http_test_varz_strict(#{port := Port}) ->
    Url = "http://localhost:"++integer_to_list(Port)++"/imetrics/varz:get",
    application:set_env(imetrics, strict_openmetrics_compat, true),
    {ok, {Status, _Headers, Body}} = 
        httpc:request(get, {Url, []}, [], []),
    application:unset_env(imetrics, strict_openmetrics_compat),
    {_Vsn, Code, _Friendly} = Status,

    Res = ["stats max:10 min:10 n:1 sum:10 sum2:100"],
    Res2 = string:join(Res, "\n") ++ "\n",
    [
        ?_assertEqual(200, Code),
        ?_assertEqual(Res2, Body)
    ].

http_test_openmetrics_strict(#{port := Port}) ->
    Url = "http://localhost:"++integer_to_list(Port)++"/metrics",
    application:set_env(imetrics, strict_openmetrics_compat, true),
    {ok, {Status, _Headers, Body}} = 
        httpc:request(get, {Url, []}, [], []),
    application:unset_env(imetrics, strict_openmetrics_compat),
    {_Vsn, Code, _Friendly} = Status,

    Res = [
        "# TYPE counter counter",
        "counter{} 1",
        "# TYPE counter2 counter",
        "counter2{} 1 # {testLabel=\"testLabelValue\"} 3 1262304000",
        "# TYPE counter:tuple:colon counter",
        "counter:tuple:colon{} 1",
        "# TYPE counter_tuple counter",
        "counter_tuple{} 1",
        "# TYPE mapped_counter counter",
        "mapped_counter{map_key=\"key\"} 1",
        "# TYPE tagged_counter counter",
        "tagged_counter{tag1=\"val1\"} 1",
        "# TYPE tagged_counter2 counter",
        "tagged_counter2{tag2=\"val1\"} 1 # {} 1.23 946684800",
        "# TYPE fn_gauge gauge",
        "fn_gauge{} 1.5",
        "# TYPE gauge gauge",
        "gauge{} 1.5",
        "# TYPE gauge2 gauge",
        "gauge2{} 0.6 # {} 0.57 946684800",
        "# TYPE mapped_gauge gauge",
        "mapped_gauge{key=\"value\"} 1.5",
        "# EOF"],
    Res2 = string:join(Res, "\n") ++ "\n",
    [
        ?_assertEqual(Res2, Body),
        ?_assertEqual(200, Code)
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
