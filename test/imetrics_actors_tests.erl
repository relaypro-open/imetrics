-module(imetrics_actors_tests).
-include_lib("eunit/include/eunit.hrl").

start() ->
    application:load(imetrics),
    application:set_env(imetrics, http_server_port, 8086),
    application:ensure_all_started(imetrics),
    #{}.

stop(_Fixture) ->
    application:stop(imetrics).

room_sensors_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun room_sensors_test/1}.

room_sensors_test(_) ->
    Actor =
                fun L() ->
                        receive {mfa, M, F, A} ->
                                    erlang:apply(M, F, A),
                                    L();
                                stop ->
                                    ok
                        end
                end,
    Kitchen = spawn_link(Actor),
    Bedroom = spawn_link(Actor),
    Garage = spawn_link(Actor),

    [
        ?_assertMatch(ok, imetrics_actors:new_guild(room_sensors)),
        ?_assertMatch({mfa, _, _, _}, Kitchen ! {mfa, imetrics_actors, set_gauge, [room_sensors, temperature, 76]}),
        ?_assertMatch({mfa, _, _, _}, Kitchen ! {mfa, imetrics_actors, set_gauge, [room_sensors, occupants, 2]}),
        ?_assertMatch({mfa, _, _, _}, Bedroom ! {mfa, imetrics_actors, set_gauge, [room_sensors, occupants, 0]}),
        ?_assertMatch({mfa, _, _, _}, Garage ! {mfa, imetrics_actors, set_gauge, [room_sensors, temperature, 89]}),
        ?_assertMatch({mfa, _, _, _}, Garage ! {mfa, imetrics_actors, set_gauge, [room_sensors, occupants, 1]}),
        ?_assertMatch(ok, timer:sleep(2)),
        ?_assertMatch( [{<<"room_sensors_actor_counts">>,
                         [{<<"$dim">>,<<"name">>},
                          {<<"occupants">>,3},
                          {<<"temperature">>,2}]},
                        {<<"room_sensors_actor_sums">>,
                         [{<<"$dim">>,<<"name">>},
                          {<<"occupants">>,3},
                          {<<"temperature">>,165}]}], imetrics:get_gauges()),
        ?_assertMatch(stop, Garage ! stop),
        ?_assertMatch(ok, timer:sleep(2)),
        ?_assertMatch([{<<"room_sensors_actor_counts">>,
                        [{<<"$dim">>,<<"name">>},
                         {<<"occupants">>,2},
                         {<<"temperature">>,1}]},
                       {<<"room_sensors_actor_sums">>,
                        [{<<"$dim">>,<<"name">>},
                         {<<"occupants">>,2},
                         {<<"temperature">>,76}]}], imetrics:get_gauges())
    ].
