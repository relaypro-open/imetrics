-module(imetrics_hist_openmetrics_tests).
-include_lib("eunit/include/eunit.hrl").

start() ->
    application:ensure_all_started(imetrics),
    #{}.

stop(_Fixture) ->
    application:stop(imetrics).

new_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun new_test/1}. 

new_test(_Fixture) ->
    [
        ?_assertEqual(true, imetrics_hist_openmetrics:new(simple, [0])),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(simple_tagged, #{test_tag => "test_value"}, [0])),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(simple_range, [0,1], 2)),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(simple_range_tagged, #{test_tag => "test_value"}, [0,1], 2)),

        ?_assertEqual(true, imetrics_hist_openmetrics:new(empty, [])), %should only have +Inf bucket
        ?_assertEqual(true, imetrics_hist_openmetrics:new(longer_list, [0, 0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.4, 1, 3, 10, 100, 100000])),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(multiple_tags, #{test_tag => "test_value", another_test_tag => "another_test_value"}, [0,1,2])),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(single_range, [0,0], 1)),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(empty_range, [0,0], 0)),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(larger_empty_range, [0,100], 0)),
        ?_assertEqual({error, {badarith, check_inputs}}, imetrics_hist_openmetrics:new(illegal_range, [0,1], 1)),
        ?_assertEqual({error, {badarith, check_inputs}}, imetrics_hist_openmetrics:new(zero_range, [0,0], 2)),

        ?_assertEqual({error, {badarg, check_ets}}, imetrics_hist_openmetrics:new(oversize_range, [0, 1], 65)),
        ?_assertEqual(ok, application:set_env(imetrics, hist_max_buckets, 8)),
        ?_assertEqual({error, {badarg, check_ets}}, imetrics_hist_openmetrics:new(oversize_dynamic, [1, 2, 3, 4, 5, 6, 7, 8, 9])),
        ?_assertEqual(ok, application:set_env(imetrics, hist_max_buckets, 64)),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(oversize_dynamic, [1, 2, 3, 4, 5, 6, 7, 8, 9]))
    ].

add_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun add_test/1}. 

add_test(_Fixture) ->
    [
        ?_assertEqual(true, imetrics_hist_openmetrics:new(simple, [0])),
        ?_assertEqual({1,1}, imetrics_hist_openmetrics:add(simple, -1)),
        ?_assertEqual({1,2}, imetrics_hist_openmetrics:add(simple, -1)),
        ?_assertEqual({2,1}, imetrics_hist_openmetrics:add(simple, 1)),
        ?_assertEqual({1,3}, imetrics_hist_openmetrics:add(simple, 0)),
        ?_assertEqual({error, {badarg, check_ets}}, imetrics_hist_openmetrics:add(fake_hist, 0)),
        ?_assertEqual({error, {badarg, check_ets}}, imetrics_hist_openmetrics:add(simple, #{bad_tag => "bad"}, 0)),

        %test adding to a tagged metric
        ?_assertEqual(true, imetrics_hist_openmetrics:new(tagged, #{tag => "one"}, [0, 1, 2])),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(tagged, #{tag => "two"}, [0, 0.5, 1])),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(tagged, #{another_tag => "one"}, [0, 1, 2])),
        ?_assertEqual({2,1}, imetrics_hist_openmetrics:add(tagged, #{tag => "one"}, 0.25)), %test independence
        ?_assertEqual({2,1}, imetrics_hist_openmetrics:add(tagged, #{tag => "two"}, 0.25)),
        ?_assertEqual({2,1}, imetrics_hist_openmetrics:add(tagged, #{another_tag => "one"}, 0.25)),
        ?_assertEqual({2,2}, imetrics_hist_openmetrics:add(tagged, #{tag => "one"}, 0.75)), %test different thresholds
        ?_assertEqual({3,1}, imetrics_hist_openmetrics:add(tagged, #{tag => "two"}, 0.75)),
        ?_assertEqual({1,1}, imetrics_hist_openmetrics:add(tagged, #{another_tag => "one"}, 0))
    ].

get_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun get_test/1}.

get_test(_Fixture) ->
    [
        ?_assertEqual(true, imetrics_hist_openmetrics:new(simple, [0])),
        ?_assertEqual([{#{le => <<"0">>}, 0},{#{le => <<"+Inf">>}, 0}], imetrics_hist_openmetrics:get_hist(simple)),
        ?_assertEqual([], imetrics_hist_openmetrics:get_hist(fake_hist)),

        ?_assertEqual([{<<"simple">>, [{#{le => <<"0">>}, 0},{#{le => <<"+Inf">>}, 0}]}], imetrics_hist_openmetrics:get_all()),

        ?_assertEqual({1,1}, imetrics_hist_openmetrics:add(simple, 0)),
        ?_assertEqual([{#{le => <<"0">>}, 1},{#{le => <<"+Inf">>}, 1}], imetrics_hist_openmetrics:get_hist(simple)),
        ?_assertEqual({2,1}, imetrics_hist_openmetrics:add(simple, 1)),
        ?_assertEqual([{#{le => <<"0">>}, 1},{#{le => <<"+Inf">>}, 2}], imetrics_hist_openmetrics:get_hist(simple)),
        
        ?_assertEqual(true, imetrics_hist_openmetrics:new(tagged, #{tag => "one"}, [0, 1, 2])),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(tagged, #{tag => "two"}, [1, 1.01, 1.1, 1.5, 2, 10])),
        ?_assertEqual([{#{le => <<"0">>, tag => <<"one">>}, 0},
                       {#{le => <<"1">>, tag => <<"one">>}, 0},
                       {#{le => <<"2">>, tag => <<"one">>}, 0},
                       {#{le => <<"+Inf">>, tag => <<"one">>}, 0}], imetrics_hist_openmetrics:get_hist(tagged, #{tag => "one"})),
        ?_assertEqual([{#{le => <<"1">>, tag => <<"two">>}, 0},
                       {#{le => <<"1.01">>, tag => <<"two">>}, 0},
                       {#{le => <<"1.1">>, tag => <<"two">>}, 0},
                       {#{le => <<"1.5">>, tag => <<"two">>}, 0},
                       {#{le => <<"2">>, tag => <<"two">>}, 0},
                       {#{le => <<"10">>, tag => <<"two">>}, 0},
                       {#{le => <<"+Inf">>, tag => <<"two">>}, 0}], imetrics_hist_openmetrics:get_hist(tagged, #{tag => "two"})),
        ?_assertEqual([], imetrics_hist_openmetrics:get_hist(tagged)),
        ?_assertEqual([{<<"tagged">>,
                        [{#{le => <<"1">>, tag => <<"two">>}, 0},
                        {#{le => <<"1.01">>, tag => <<"two">>}, 0},
                        {#{le => <<"1.1">>, tag => <<"two">>}, 0},
                        {#{le => <<"1.5">>, tag => <<"two">>}, 0},
                        {#{le => <<"2">>, tag => <<"two">>}, 0},
                        {#{le => <<"10">>, tag => <<"two">>}, 0},
                        {#{le => <<"+Inf">>, tag => <<"two">>}, 0},
                        {#{le => <<"0">>, tag => <<"one">>}, 0},
                        {#{le => <<"1">>, tag => <<"one">>}, 0},
                        {#{le => <<"2">>, tag => <<"one">>}, 0},
                        {#{le => <<"+Inf">>, tag => <<"one">>}, 0}]},
                       {<<"simple">>,
                        [{#{le => <<"0">>}, 1},
                        {#{le => <<"+Inf">>}, 2}]}], imetrics_hist_openmetrics:get_all()),

        ?_assertEqual({2,1}, imetrics_hist_openmetrics:add(tagged, #{tag => "one"}, 1)),
        ?_assertEqual({3,1}, imetrics_hist_openmetrics:add(tagged, #{tag => "one"}, 2)),
        ?_assertEqual({3,2}, imetrics_hist_openmetrics:add(tagged, #{tag => "one"}, 2)),
        ?_assertEqual({4,1}, imetrics_hist_openmetrics:add(tagged, #{tag => "one"}, 5)),
        ?_assertEqual([{<<"tagged">>,
                        [{#{le => <<"1">>, tag => <<"two">>}, 0},
                        {#{le => <<"1.01">>, tag => <<"two">>}, 0},
                        {#{le => <<"1.1">>, tag => <<"two">>}, 0},
                        {#{le => <<"1.5">>, tag => <<"two">>}, 0},
                        {#{le => <<"2">>, tag => <<"two">>}, 0},
                        {#{le => <<"10">>, tag => <<"two">>}, 0},
                        {#{le => <<"+Inf">>, tag => <<"two">>}, 0},
                        {#{le => <<"0">>, tag => <<"one">>}, 0},
                        {#{le => <<"1">>, tag => <<"one">>}, 1},
                        {#{le => <<"2">>, tag => <<"one">>}, 3},
                        {#{le => <<"+Inf">>, tag => <<"one">>}, 4}]},
                       {<<"simple">>,
                        [{#{le => <<"0">>}, 1},
                        {#{le => <<"+Inf">>}, 2}]}], imetrics_hist_openmetrics:get_all()),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(range, [15, 20], 6)),
        ?_assertEqual([{#{le => <<"15">>}, 0},
                       {#{le => <<"16.0">>}, 0},
                       {#{le => <<"17.0">>}, 0},
                       {#{le => <<"18.0">>}, 0},
                       {#{le => <<"19.0">>}, 0},
                       {#{le => <<"20">>}, 0},
                       {#{le => <<"+Inf">>}, 0}], imetrics_hist_openmetrics:get_hist(range)),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(large_range, [0, 1], 21)),
        ?_assertEqual([{#{le => <<"0">>},0},
                       {#{le => <<"0.049999999999999684">>},0},
                       {#{le => <<"0.09999999999999969">>},0},
                       {#{le => <<"0.1499999999999997">>},0},
                       {#{le => <<"0.19999999999999968">>},0},
                       {#{le => <<"0.24999999999999967">>},0},
                       {#{le => <<"0.29999999999999966">>},0},
                       {#{le => <<"0.34999999999999964">>},0},
                       {#{le => <<"0.39999999999999963">>},0},
                       {#{le => <<"0.4499999999999996">>},0},
                       {#{le => <<"0.4999999999999996">>},0},
                       {#{le => <<"0.5499999999999996">>},0},
                       {#{le => <<"0.5999999999999996">>},0},
                       {#{le => <<"0.6499999999999997">>},0},
                       {#{le => <<"0.6999999999999997">>},0},
                       {#{le => <<"0.7499999999999998">>},0},
                       {#{le => <<"0.7999999999999998">>},0},
                       {#{le => <<"0.8499999999999999">>},0},
                       {#{le => <<"0.8999999999999999">>},0},
                       {#{le => <<"0.95">>},0},
                       {#{le => <<"1">>},0},
                       {#{le => <<"+Inf">>},0}], imetrics_hist_openmetrics:get_hist(large_range))
    ].

exemplar_test_() ->
    {setup, 
        fun start/0,
        fun stop/1,
        fun exemplar_test/1}.

exemplar_test(_Fixture) ->
    [
        %specifically focuses on testing the elements of exemplars unique to histograms.
        %more through tests of exemplars, including testing all argument variants, can be found in imetrics_tests.erl.
        ?_assertEqual(true, imetrics_hist_openmetrics:new(simple, [0])),
        ?_assertEqual(true, imetrics_hist_openmetrics:set_exemplar(simple, 0)),
        ?_assertEqual(true, imetrics_hist_openmetrics:set_exemplar(simple, 1)),
        ?_assertMatch({0, #{}, _}, imetrics:get_exemplar(#{'__name__' => <<"simple">>, le => <<"0">>})),
        ?_assertMatch({1, #{}, _}, imetrics:get_exemplar(#{'__name__' => <<"simple">>, le => <<"+Inf">>})),
        ?_assertEqual({error, {badarg, check_ets}}, imetrics_hist_openmetrics:set_exemplar(fake_hist, 1)),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(simple_range_tagged, #{test_tag => "test_value"}, [0,1], 2)),
        ?_assertEqual(true, imetrics_hist_openmetrics:set_exemplar(simple_range_tagged, #{test_tag => "test_value"}, 0.8)),
        ?_assertMatch({0.8, #{}, _}, imetrics:get_exemplar(#{'__name__' => <<"simple_range_tagged">>, le => <<"1">>, test_tag => <<"test_value">>}))
    ].