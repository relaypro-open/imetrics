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

multigauge_multidim_test_() ->
    {setup, fun start/0, fun stop/1, fun multigauge_multidim_test/1}.

multigauge_multidim_test(_) ->
    imetrics:set_multigauge(multigauge_multidim_test, {dim1, dim2}, fun() -> [{{<<"test1">>, <<"test2">>}, 4}] end),
    Data = imetrics:get_with_types(),
    [
        ?_assertEqual({gauge, [{#{ dim1 => <<"test1">>, dim2 => <<"test2">> }, 4}]}, proplists:get_value(<<"multigauge_multidim_test">>, Data))
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

info_test_() ->
    {setup, fun start/0,
        fun stop/1,
        fun info_test/1}.

info_test(_Fixture) ->
    [
        ?_assertEqual(true, imetrics:set_info(empty_map, #{})),
        ?_assertEqual(true, imetrics:set_info(simple_map, #{elem_1 => val_1})),
        ?_assertEqual(true, imetrics:set_info(simple_map, #{elem_1 => val_2})),
        ?_assertEqual([{<<"empty_map">>, {info, [{#{}, 1}]}}, {<<"simple_map">>, {info, [{#{elem_1 => <<"val_2">>}, 1}]}}], imetrics:get_with_types()),
        ?_assertEqual(true, imetrics:set_info(simple_map, #{elem_1 => val_3, elem_2 => val_1})),
        ?_assertEqual([{<<"empty_map">>, {info, [{#{}, 1}]}}, {<<"simple_map">>, {info, [{#{elem_1 => <<"val_3">>, elem_2 => <<"val_1">>}, 1}]}}], imetrics:get_with_types())
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
    imetrics:set_exemplar(l, #{name => "test1"}, 1),
    imetrics:set_exemplar(l, #{name => "test2"}, 2),

    ?_assertMatch({1, #{}, _}, imetrics:get_exemplar(#{'__name__' => <<"a">>})),
    ?_assertMatch({2, #{}, _}, imetrics:get_exemplar(#{'__name__' => <<"b">>, c => <<"1">>})),
    Test3Map = #{d => <<"2">>, e => <<"aa">>},
    ?_assertMatch({2, Test3Map, _}, imetrics:get_exemplar(#{'__name__' => <<"c">>})),
    ?_assertMatch({3, #{}, 946684800}, imetrics:get_exemplar(#{'__name__' => <<"f">>})),
    Test5Map = #{j => <<"k">>},
    ?_assertMatch({3, Test5Map, _}, imetrics:get_exemplar(#{'__name__' => <<"g">>, h => <<"1">>, i => <<"2">>})),
    ?_assertMatch({4, #{}, 978307200}, imetrics:get_exemplar(#{'__name__' => <<"g">>, h => <<"2">>})),
    Test7Map = #{j => <<"bb">>},
    ?_assertMatch({5, Test7Map, 1009843200}, imetrics:get_exemplar(#{'__name__' => <<"i">>})),
    Test8Map = #{m => <<"cc">>},
    ?_assertMatch({6, Test8Map, 1577836800}, imetrics:get_exemplar(#{'__name__' => <<"k">>, l => <<"3">>})),
    
    ?_assertEqual(true, imetrics:set_exemplar(k, #{l => 3}, 0.1, 1893456000)),
    imetrics:set_exemplar(k, #{l => 3}, 0.1, 1893456000),
    ?_assertMatch({0.1, #{}, 1893456000}, imetrics:get_exemplar(#{'__name__' => <<"k">>, l => <<"3">>})),

    ?_assertMatch({1, #{}, _}, imetrics:get_exemplar(#{name => <<"test1">>, '__name__' => <<"l">>})),
    ?_assertMatch({2, #{}, _}, imetrics:get_exemplar(#{name => <<"test2">>, '__name__' => <<"l">>})).


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
    imetrics:set_gauge(fn_gauge_timeout, fun() -> timer:sleep(6000), 1 end),
    imetrics:add({counter, tuple}),
    application:set_env(imetrics, separator, <<":">>),
    imetrics:add({counter, tuple, colon}),
    application:unset_env(imetrics, separator),
    Stats = imetrics:stats(stats),
    Stats2 = imetrics_stats:update(10, Stats),
    imetrics:set_stats(stats, Stats2),

    imetrics:set_multigauge(multigauge, single_dim, fun () -> [{value1, 1}, {value3, 3}] end),
    imetrics:set_multigauge(multigauge_2, {multi_dim_1, multi_dim_2}, fun () -> [{{value1, value2}, 1}, {{value3, value4}, 3}] end),

    imetrics:add(counter2),
    imetrics:add(tagged_counter2, #{tag2 => "val1"}),
    imetrics:set_exemplar(counter2, 3, #{test_label => test_label_value}, 1262304000),
    imetrics:set_exemplar(tagged_counter2, #{tag2 => "val1"}, 1.23, 946684800),
    imetrics:set_exemplar(tagged_counter2, 100, 946684800),

    imetrics:add(counter3_invalid_exemplar),
    imetrics:set_exemplar(counter3_invalid_exemplar, 3, #{'9invalid_label' => valid_value}, 12623040000),
    imetrics:add(counter4_invalid_exemplar),
    imetrics:set_exemplar(counter4_invalid_exemplar, 3, #{valid_label => "Invalid \"Value\""}, 12623040000),

    imetrics_hist_openmetrics:new(hist, [0,0.01,0.1,0.5,1,5,10]),
    imetrics_hist_openmetrics:new(tagged_hist, #{hist_tag => "one"}, [0,1], 5),
    imetrics_hist_openmetrics:new(tagged_hist, #{hist_tag => "two"}, [0,10], 21),
    imetrics_hist_openmetrics:add(hist, 0.3),
    imetrics_hist_openmetrics:add(hist, 0.3),
    imetrics_hist_openmetrics:add(hist, 1),
    imetrics_hist_openmetrics:add(hist, 11),
    imetrics_hist_openmetrics:add(hist, 11),
    imetrics_hist_openmetrics:add(tagged_hist, #{hist_tag => "one"}, 0.3),
    imetrics_hist_openmetrics:add(tagged_hist, #{hist_tag => "two"}, 7),
    imetrics_hist_openmetrics:set_exemplar(hist, 0.4, 1262304000),
    imetrics_hist_openmetrics:set_exemplar(hist, 0.6, 1893456000),
    imetrics_hist_openmetrics:set_exemplar(tagged_hist, #{hist_tag => "one"}, 0.58, 946684800),
    imetrics_hist_openmetrics:set_exemplar(tagged_hist, #{hist_tag => "two"}, 6.8, 1893456000),
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
        ?_assertEqual(-1, proplists:get_value(<<"fn_gauge_timeout">>, Data)),
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
    http_test_varz_strict(#{port => Port}) ++ http_test_openmetrics_strict(#{port => Port}) ++
    http_test_openmetrics_exemplars(#{port => Port}).

http_test_varz(#{port := Port}) ->
    Url = "http://localhost:"++integer_to_list(Port)++"/imetrics/varz:get",
    {ok, {Status, _Headers, Body}} = 
        httpc:request(get, {Url, []}, [], []),
    {_Vsn, Code, _Friendly} = Status,

    Res = ["counter 1",
        "counter2 1",
        "counter3_invalid_exemplar 1",
        "counter4_invalid_exemplar 1",
        "counter:tuple:colon 1",
        "counter_tuple 1",
        "mapped_counter key:1",
        "fn_gauge 1.5",
        "fn_gauge_timeout -1",
        "gauge 1.5",
        "mapped_gauge $dim:key value:1.5",
        "multigauge $dim:single_dim value1:1 value3:3",
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
        "counter2{} 1",
        "# TYPE counter3_invalid_exemplar counter",
        "counter3_invalid_exemplar{} 1",
        "# TYPE counter4_invalid_exemplar counter",
        "counter4_invalid_exemplar{} 1",
        "# TYPE counter:tuple:colon counter",
        "counter:tuple:colon{} 1",
        "# TYPE counter_tuple counter",
        "counter_tuple{} 1",
        "# TYPE imetrics_metric_fun_timeout counter",
        "imetrics_metric_fun_timeout{metric=\"fn_gauge_timeout\"} 1",
        "# TYPE mapped_counter counter",
        "mapped_counter{map_key=\"key\"} 1",
        "# TYPE tagged_counter counter",
        "tagged_counter{tag1=\"val1\"} 1",
        "# TYPE tagged_counter2 counter",
        "tagged_counter2{tag2=\"val1\"} 1",
        "# TYPE fn_gauge gauge",
        "fn_gauge{} 1.5",
        "# TYPE fn_gauge_timeout gauge",
        "fn_gauge_timeout{} -1",
        "# TYPE gauge gauge",
        "gauge{} 1.5",
        "# TYPE mapped_gauge gauge",
        "mapped_gauge{key=\"value\"} 1.5",
        "# TYPE multigauge gauge",
        "multigauge{single_dim=\"value1\"} 1",
        "multigauge{single_dim=\"value3\"} 3",
        "# TYPE multigauge_2 gauge",
        "multigauge_2{multi_dim_1=\"value1\",multi_dim_2=\"value2\"} 1",
        "multigauge_2{multi_dim_1=\"value3\",multi_dim_2=\"value4\"} 3",
        "# TYPE stats unknown",
        "stats{map_key=\"max\"} 10",
        "stats{map_key=\"min\"} 10",
        "stats{map_key=\"n\"} 1",
        "stats{map_key=\"sum\"} 10",
        "stats{map_key=\"sum2\"} 100",
        "# TYPE tagged_hist histogram",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"0.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"1.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"1.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"2.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"2.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"3.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"3.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"4.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"4.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"5.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"5.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"6.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"6.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"7.0\"} 1 # {} 6.8 1893456000",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"7.5\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"8.0\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"8.5\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"9.0\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"9.5\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"10\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"+Inf\"} 1",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"0\"} 0",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"0.25\"} 0",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"0.5\"} 1",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"0.75\"} 1 # {} 0.58 946684800",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"1\"} 1",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"+Inf\"} 1",
        "# TYPE hist histogram",
        "hist_bucket{le=\"0\"} 0",
        "hist_bucket{le=\"0.01\"} 0",
        "hist_bucket{le=\"0.1\"} 0",
        "hist_bucket{le=\"0.5\"} 2 # {} 0.4 1262304000",
        "hist_bucket{le=\"1\"} 3 # {} 0.6 1893456000",
        "hist_bucket{le=\"5\"} 3",
        "hist_bucket{le=\"10\"} 3",
        "hist_bucket{le=\"+Inf\"} 5",
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
        "counter2{} 1",
        "# TYPE counter3_invalid_exemplar counter",
        "counter3_invalid_exemplar{} 1",
        "# TYPE counter4_invalid_exemplar counter",
        "counter4_invalid_exemplar{} 1",
        "# TYPE counter:tuple:colon counter",
        "counter:tuple:colon{} 1",
        "# TYPE counter_tuple counter",
        "counter_tuple{} 1",
        "# TYPE imetrics_metric_fun_timeout counter",
        "imetrics_metric_fun_timeout{metric=\"fn_gauge_timeout\"} 3",
        "# TYPE mapped_counter counter",
        "mapped_counter{map_key=\"key\"} 1",
        "# TYPE tagged_counter counter",
        "tagged_counter{tag1=\"val1\"} 1",
        "# TYPE tagged_counter2 counter",
        "tagged_counter2{tag2=\"val1\"} 1",
        "# TYPE fn_gauge gauge",
        "fn_gauge{} 1.5",
        "# TYPE fn_gauge_timeout gauge",
        "fn_gauge_timeout{} -1",
        "# TYPE gauge gauge",
        "gauge{} 1.5",
        "# TYPE mapped_gauge gauge",
        "mapped_gauge{key=\"value\"} 1.5",
        "# TYPE multigauge gauge",
        "multigauge{single_dim=\"value1\"} 1",
        "multigauge{single_dim=\"value3\"} 3",
        "# TYPE multigauge_2 gauge",
        "multigauge_2{multi_dim_1=\"value1\",multi_dim_2=\"value2\"} 1",
        "multigauge_2{multi_dim_1=\"value3\",multi_dim_2=\"value4\"} 3",
        "# TYPE tagged_hist histogram",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"0.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"1.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"1.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"2.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"2.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"3.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"3.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"4.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"4.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"5.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"5.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"6.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"6.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"7.0\"} 1 # {} 6.8 1893456000",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"7.5\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"8.0\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"8.5\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"9.0\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"9.5\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"10\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"+Inf\"} 1",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"0\"} 0",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"0.25\"} 0",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"0.5\"} 1",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"0.75\"} 1 # {} 0.58 946684800",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"1\"} 1",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"+Inf\"} 1",
        "# TYPE hist histogram",
        "hist_bucket{le=\"0\"} 0",
        "hist_bucket{le=\"0.01\"} 0",
        "hist_bucket{le=\"0.1\"} 0",
        "hist_bucket{le=\"0.5\"} 2 # {} 0.4 1262304000",
        "hist_bucket{le=\"1\"} 3 # {} 0.6 1893456000",
        "hist_bucket{le=\"5\"} 3",
        "hist_bucket{le=\"10\"} 3",
        "hist_bucket{le=\"+Inf\"} 5",
        "# EOF"],
    Res2 = string:join(Res, "\n") ++ "\n",
    [
        ?_assertEqual(Res2, Body),
        ?_assertEqual(200, Code)
    ].

http_test_openmetrics_exemplars(#{port := Port}) ->
    Url = "http://localhost:"++integer_to_list(Port)++"/metrics",
    application:set_env(imetrics, strict_openmetrics_compat, true),
    application:set_env(imetrics, openmetrics_exemplar_compat, true),
    {ok, {Status, _Headers, Body}} = 
        httpc:request(get, {Url, []}, [], []),
    application:unset_env(imetrics, strict_openmetrics_compat),
    {_Vsn, Code, _Friendly} = Status,

    Res = [
        "# TYPE counter counter",
        "counter_total{} 1",
        "# TYPE counter2 counter",
        "counter2_total{} 1 # {test_label=\"test_label_value\"} 3 1262304000",
        "# TYPE counter3_invalid_exemplar counter",
        "counter3_invalid_exemplar_total{} 1",
        "# TYPE counter4_invalid_exemplar counter",
        "counter4_invalid_exemplar_total{} 1",
        "# TYPE counter:tuple:colon counter",
        "counter:tuple:colon_total{} 1",
        "# TYPE counter_tuple counter",
        "counter_tuple_total{} 1",
        "# TYPE imetrics_metric_fun_timeout counter",
        "imetrics_metric_fun_timeout_total{metric=\"fn_gauge_timeout\"} 4",
        "# TYPE mapped_counter counter",
        "mapped_counter_total{map_key=\"key\"} 1",
        "# TYPE tagged_counter counter",
        "tagged_counter_total{tag1=\"val1\"} 1",
        "# TYPE tagged_counter2 counter",
        "tagged_counter2_total{tag2=\"val1\"} 1 # {} 1.23 946684800",
        "# TYPE fn_gauge gauge",
        "fn_gauge{} 1.5",
        "# TYPE fn_gauge_timeout gauge",
        "fn_gauge_timeout{} -1",
        "# TYPE gauge gauge",
        "gauge{} 1.5",
        "# TYPE mapped_gauge gauge",
        "mapped_gauge{key=\"value\"} 1.5",
        "# TYPE multigauge gauge",
        "multigauge{single_dim=\"value1\"} 1",
        "multigauge{single_dim=\"value3\"} 3",
        "# TYPE multigauge_2 gauge",
        "multigauge_2{multi_dim_1=\"value1\",multi_dim_2=\"value2\"} 1",
        "multigauge_2{multi_dim_1=\"value3\",multi_dim_2=\"value4\"} 3",
        "# TYPE tagged_hist histogram",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"0.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"1.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"1.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"2.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"2.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"3.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"3.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"4.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"4.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"5.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"5.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"6.0\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"6.5\"} 0",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"7.0\"} 1 # {} 6.8 1893456000",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"7.5\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"8.0\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"8.5\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"9.0\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"9.5\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"10\"} 1",
        "tagged_hist_bucket{hist_tag=\"two\",le=\"+Inf\"} 1",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"0\"} 0",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"0.25\"} 0",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"0.5\"} 1",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"0.75\"} 1 # {} 0.58 946684800",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"1\"} 1",
        "tagged_hist_bucket{hist_tag=\"one\",le=\"+Inf\"} 1",
        "# TYPE hist histogram",
        "hist_bucket{le=\"0\"} 0",
        "hist_bucket{le=\"0.01\"} 0",
        "hist_bucket{le=\"0.1\"} 0",
        "hist_bucket{le=\"0.5\"} 2 # {} 0.4 1262304000",
        "hist_bucket{le=\"1\"} 3 # {} 0.6 1893456000",
        "hist_bucket{le=\"5\"} 3",
        "hist_bucket{le=\"10\"} 3",
        "hist_bucket{le=\"+Inf\"} 5",
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
    imetrics:hist(test, [1, 10]),
    imetrics:hist(test2, [1, 10]),
    F.

ticktock_test(_Fixture) ->
    [
        ?_assertEqual(0, (fun() ->
                    Tick = imetrics:tick(test, second),
                    timer:sleep(5),
                    imetrics:tock(Tick)
            end)()),
        ?_assertEqual(0, (fun() ->
                    Tick = imetrics:tick(test, second),
                    timer:sleep(5),
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
     ?_assertMatch(0, (fun() ->
                                     {Ref, Ticks} = imetrics:tick_s(#{}, test, second),
                                     timer:sleep(5),
                                     {Result, Ticks2} = imetrics:tock_s(Ticks, Ref),
                                     0 = map_size(Ticks2),
                                     Result
                             end)()),
     ?_assertMatch(0, (fun() ->
                                     {_Ref, Ticks} = imetrics:tick_s(#{}, test, second),
                                     timer:sleep(5),
                                     {Result, Ticks2} = imetrics:tock_s(Ticks, test),
                                     0 = map_size(Ticks2),
                                     Result
                             end)())
    ].
