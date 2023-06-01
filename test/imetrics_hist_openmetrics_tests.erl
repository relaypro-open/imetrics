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
        ?_assertEqual(true, imetrics_hist_openmetrics:new(larger_range, [0,1000], 101)),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(decimal_range, [0, 1], 1001)),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(empty_range, [0,0], 0)),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(larger_empty_range, [0,100], 0)),
        ?_assertEqual({error, {badarith, check_inputs}}, imetrics_hist_openmetrics:new(illegal_range, [0,1], 1)),
        ?_assertEqual({error, {badarith, check_inputs}}, imetrics_hist_openmetrics:new(zero_range, [0,0], 2))
    ].

add_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun add_test/1}. 

add_test(_Fixture) ->
    [
        ?_assertEqual(true, imetrics_hist_openmetrics:new(simple, [0])),
        ?_assertEqual(1, imetrics_hist_openmetrics:add(simple, -1)),
        ?_assertEqual(2, imetrics_hist_openmetrics:add(simple, -1)),
        ?_assertEqual(1, imetrics_hist_openmetrics:add(simple, 1)),
        ?_assertEqual(3, imetrics_hist_openmetrics:add(simple, 0)),
        ?_assertEqual({error, {badarg, check_ets}}, imetrics_hist_openmetrics:add(fake_hist, 0)),
        ?_assertEqual({error, {badarg, check_ets}}, imetrics_hist_openmetrics:add(simple, #{bad_tag => "bad"}, 0)),

        %test adding to a tagged metric
        ?_assertEqual(true, imetrics_hist_openmetrics:new(tagged, #{tag => "one"}, [0, 1, 2])),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(tagged, #{tag => "two"}, [0, 0.5, 1])),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(tagged, #{another_tag => "one"}, [0, 1, 2])),
        ?_assertEqual(1, imetrics_hist_openmetrics:add(tagged, #{tag => "one"}, 0.25)), %test independence
        ?_assertEqual(1, imetrics_hist_openmetrics:add(tagged, #{tag => "two"}, 0.25)),
        ?_assertEqual(1, imetrics_hist_openmetrics:add(tagged, #{another_tag => "one"}, 0.25)),
        ?_assertEqual(2, imetrics_hist_openmetrics:add(tagged, #{tag => "one"}, 0.75)), %test different thresholds
        ?_assertEqual(1, imetrics_hist_openmetrics:add(tagged, #{tag => "two"}, 0.75)),
        ?_assertEqual(1, imetrics_hist_openmetrics:add(tagged, #{another_tag => "one"}, 0))
    ].

get_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun get_test/1}.

get_test(_Fixture) ->
    [
        %doesn't test get_hist(Identifier) explicitly, as it's called by get_all(), and it isn't expected to be called by users directly.
        ?_assertEqual(true, imetrics_hist_openmetrics:new(simple, [0])),
        ?_assertEqual([{#{le => <<"0">>}, 0},{#{le => <<"+Inf">>}, 0}], imetrics_hist_openmetrics:get_hist(simple)),
        ?_assertEqual([], imetrics_hist_openmetrics:get_hist(fake_hist)),

        ?_assertEqual([{<<"simple">>, [{#{le => <<"0">>}, 0},{#{le => <<"+Inf">>}, 0}]}], imetrics_hist_openmetrics:get_all()),

        ?_assertEqual(1, imetrics_hist_openmetrics:add(simple, 0)),
        ?_assertEqual([{#{le => <<"0">>}, 1},{#{le => <<"+Inf">>}, 1}], imetrics_hist_openmetrics:get_hist(simple)),
        ?_assertEqual(1, imetrics_hist_openmetrics:add(simple, 1)),
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

        ?_assertEqual(1, imetrics_hist_openmetrics:add(tagged, #{tag => "one"}, 1)),
        ?_assertEqual(1, imetrics_hist_openmetrics:add(tagged, #{tag => "one"}, 2)),
        ?_assertEqual(2, imetrics_hist_openmetrics:add(tagged, #{tag => "one"}, 2)),
        ?_assertEqual(1, imetrics_hist_openmetrics:add(tagged, #{tag => "one"}, 5)),
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
                        {#{le => <<"+Inf">>}, 2}]}], imetrics_hist_openmetrics:get_all())
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