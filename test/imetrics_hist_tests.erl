-module(imetrics_hist_tests).
-include_lib("eunit/include/eunit.hrl").

start() ->
    application:start(inets),
    application:start(imetrics).

stop(_Fixture) ->
    application:stop(imetrics),
    application:stop(inets).

compute_bucket_test_() ->
    Tests = [
        {0, [0,10], 10, -1},
        {11, [0, 10], 10, 100},
        {6, [0, 10], 10, 5.5},
        {10, [0, 10], 10, 9.9999},

        {1, [0, 1000], 50, 0},
        {1, [0, 1000], 50, 15},
        {2, [0, 1000], 50, 20},
        {3, [0, 1000], 50, 40},
        {4, [0, 1000], 50, 60},
        {15, [0, 1000], 50, 285},
        {51, [0, 1000], 50, 1000}
    ],
    [ ?_assertEqual(Expect,
            imetrics_hist:compute_bucket(Range, NumBuckets, Value)) ||
        {Expect, Range, NumBuckets, Value} <- Tests ].

misuse_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun misuse_test/1}. 

misuse_test(_) ->
    imetrics_hist:new(div0, [0, 1], 0),
    [
        ?_assertEqual({error,{badarg,check_ets}}, imetrics_hist:add(dne, 5.5)),
        ?_assertEqual({error,{badarith,check_inputs}}, imetrics_hist:add(div0, 0.4))
    ].
