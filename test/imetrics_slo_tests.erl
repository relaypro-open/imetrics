-module(imetrics_slo_tests).
-include_lib("eunit/include/eunit.hrl").

start() ->
    application:load(imetrics),
    application:start(inets),
    application:start(imetrics),
    {ok, _} = imetrics:register_slo(eunit, #{size => 128, hwm => 1024*100}).

stop(_Fixture) ->
    application:stop(imetrics),
    application:stop(inets).

%% add tests
slo_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun slo_test/1}. 

slo_test(_Fixture) ->
    [
     ?_assertMatch(0, imetrics_slo:get(eunit, <<"uid">>, <<"test_metric">>)),
     ?_assertMatch(ok, imetrics_slo:add(eunit, <<"uid">>, <<"test_metric">>)),
     ?_assertMatch(1, imetrics_slo:get(eunit, <<"uid">>, <<"test_metric">>)),
     ?_assertMatch([{<<"uid">>,[{<<"$ms">>,_},{<<"test_metric">>,1}]}], imetrics_slo:dump(eunit)),
     ?_assertMatch(1, imetrics_slo:foldl_dump(eunit, fun({_, _}, C) -> C + 1 end, 0)),
     ?_assertMatch([{<<"uid">>,[{<<"$ms">>,_},{<<"test_metric">>,1}]}], imetrics_slo:dump(eunit, <<"uid">>)),
     ?_assertMatch(#{keys := 1, memory := 1064, uids := 1}, imetrics_slo:info(eunit)),
     ?_assertMatch(#{eunit := #{keys := 1, memory := 1064, uids := 1}}, imetrics_sup:slo_info()),
     ?_assertMatch(_, [ imetrics_slo:add(eunit, X, <<"test_metric">>) || X <- lists:seq(1, 1000)]),
     ?_assertMatch(#{keys := 1, memory := 97888, uids := 92}, imetrics_slo:info(eunit)),
     ?_assertMatch(ok, imetrics_slo:put(eunit, <<"uid">>, <<"test_put_metric">>, 10)),
     ?_assertMatch(10, imetrics_slo:get(eunit, <<"uid">>, <<"test_put_metric">>)),
     ?_assertMatch(ok, imetrics_slo:remove(eunit, <<"uid">>)),
     ?_assertMatch(0, imetrics_slo:get(eunit, <<"uid">>, <<"test_put_metric">>))
    ].
