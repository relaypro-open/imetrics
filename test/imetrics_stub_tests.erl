-module(imetrics_stub_tests).
-include_lib("eunit/include/eunit.hrl").

%% Run with: rebar3 as stub eunit
-ifdef(IMETRICS_STUB).

start() ->
    application:ensure_all_started(imetrics),
    #{}.

stop(_Fixture) ->
    application:stop(imetrics).

stub_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun stub_test/1}.

stub_test(_Fixture) ->
    Fun = fun() -> 1 end,
    Stats = imetrics_stats:new(),
    [
        ?_assertEqual(stub, imetrics:impl()),

        % nothing is started or allocated
        ?_assertEqual([], supervisor:which_children(imetrics_sup)),
        ?_assertEqual(undefined, ets:info(imetrics_counters)),
        ?_assertEqual(undefined, ets:info(imetrics_hist_openmetrics)),

        % counters
        ?_assertEqual(1, imetrics:add(foo)),
        ?_assertEqual(1, imetrics:add(foo, #{a => b})),
        ?_assertEqual(5, imetrics:add(foo, 5)),
        ?_assertEqual(5, imetrics:add(foo, #{a => b}, 5)),
        ?_assertEqual(1, imetrics:add_m(foo, k)),
        ?_assertEqual(5, imetrics:add_m(foo, k, 5)),
        ?_assertEqual(0, imetrics:init_counter(foo)),
        ?_assertEqual(0, imetrics:init_counter(foo, #{a => b})),
        ?_assertEqual(true, imetrics:set_exemplar(foo, 1)),
        ?_assertEqual(true, imetrics:set_exemplar(foo, #{a => b}, 1, #{}, 0, counter)),
        ?_assertEqual(true, imetrics:set_info(foo, #{a => b})),
        ?_assertEqual(<<"dim">>, imetrics:set_counter_dimension(foo, dim)),
        ?_assertEqual(5, imetrics:set_counter_dimension(foo, 5)),

        % gauges
        ?_assertEqual(1.5, imetrics:set_gauge(foo, 1.5)),
        ?_assertEqual(1.5, imetrics:set_gauge(foo, #{a => b}, 1.5)),
        ?_assertEqual(2, imetrics:set_gauge_m(foo, k, 2)),
        ?_assertEqual(2, imetrics:update_gauge(foo, 2)),
        ?_assertEqual(2, imetrics:update_gauge(foo, #{a => b}, 2)),
        ?_assertEqual(2, imetrics:update_gauge_m(foo, k, 2)),
        ?_assertEqual(Fun, imetrics:set_multigauge(foo, Fun)),
        ?_assertEqual(Fun, imetrics:set_multigauge(foo, {a, b}, Fun)),

        % stats / slo
        ?_assertEqual(Stats, imetrics:stats(foo)),
        ?_assertEqual(Stats, imetrics:set_stats(foo, Stats)),
        ?_assertEqual({ok, undefined}, imetrics:register_slo(foo, #{})),
        ?_assertEqual(acc, imetrics:foldl_slo(foo, fun(_, A) -> A end, acc)),
        ?_assertEqual([], imetrics:get_slo(foo, uid)),
        ?_assertEqual(imetrics_slo_foo, imetrics_slo:svr_ref(foo)),
        ?_assertEqual(foo, imetrics_slo:uid_name(imetrics_slo_foo)),
        ?_assertEqual(0, imetrics_slo:get(foo, uid, key)),
        ?_assertEqual(ok, imetrics_slo:add(foo, uid, key)),
        ?_assertEqual(ok, imetrics_slo:add(foo, uid, key, 2)),
        ?_assertEqual(ok, imetrics_slo:put(foo, uid, key, 2)),
        ?_assertEqual(ok, imetrics_slo:remove(foo, uid)),
        ?_assertEqual([], imetrics_slo:dump(foo)),
        ?_assertEqual([], imetrics_slo:dump(foo, uid)),
        ?_assertEqual(#{uids => 0, memory => 0, keys => 0}, imetrics_slo:info(foo)),
        ?_assertEqual(acc, imetrics_slo:foldl_dump(foo, fun(_, A) -> A end, acc)),

        % histograms
        ?_assertEqual(true, imetrics_hist_openmetrics:new(foo, [0, 1])),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(foo, #{a => b}, [0, 1])),
        ?_assertEqual(true, imetrics_hist_openmetrics:new(foo, #{a => b}, [0, 1], 2)),
        ?_assertEqual(true, imetrics:hist(foo, [0, 1])),
        ?_assertEqual(true, imetrics:hist(foo, #{a => b}, [0, 1])),
        ?_assertEqual(true, imetrics:hist(foo, [0, 1], 2)),
        ?_assertEqual(true, imetrics:hist(foo, #{a => b}, [0, 1], 2)),
        ?_assertEqual({0, 0}, imetrics_hist_openmetrics:add(foo, 1)),
        ?_assertEqual({0, 0}, imetrics_hist_openmetrics:add(foo, #{a => b}, 1)),
        ?_assertEqual(true, imetrics_hist_openmetrics:set_exemplar(foo, 1)),
        ?_assertEqual([], imetrics_hist_openmetrics:get_hist(foo)),
        ?_assertEqual([], imetrics_hist_openmetrics:get_hist(foo, #{a => b})),
        ?_assertEqual([], imetrics_hist_openmetrics:get_all()),

        % tick/tock still measure, but record nothing
        ?_assert(imetrics:tock(imetrics:tick(foo)) >= 0),
        ?_assert(imetrics:tock(imetrics:tick(foo, #{a => b}, microsecond)) >= 0),
        ?_assertEqual(42, imetrics:tock(imetrics:tick(foo), fun(_, _) -> 42 end)),
        ?_assert(imetrics:tock_as(imetrics:tick(foo), bar) >= 0),
        ?_assertMatch({D, #{}} when D >= 0,
                      begin
                          {Ref, Ticks} = imetrics:tick_s(#{}, foo, microsecond),
                          imetrics:tock_s(Ticks, Ref)
                      end),
        ?_assertMatch({D, #{}} when D >= 0,
                      begin
                          {Ref, Ticks} = imetrics:tick_s(#{}, foo, microsecond),
                          imetrics:tock_as_s(Ticks, Ref, bar)
                      end),
        ?_assertEqual(#{}, begin
                               {Ref, Ticks} = imetrics:tick_s(#{}, foo, microsecond),
                               imetrics:stop_tick_s(Ticks, Ref)
                           end),

        % actors
        ?_assertEqual(ok, imetrics_actors:new_guild(foo)),
        ?_assertEqual(ok, imetrics_actors:set_gauge(foo, bar, 1)),
        ?_assertEqual(ok, imetrics_actors:unset_gauge(foo, bar)),
        ?_assertEqual([], imetrics_actors:dump(foo, sums)),

        % vm metrics
        ?_assertEqual(ok, imetrics_vm_metrics:install()),

        % getters
        ?_assertEqual([], imetrics:get()),
        ?_assertEqual([], imetrics:get_with_types()),
        ?_assertEqual([], imetrics:get_counters()),
        ?_assertEqual([], imetrics:get_gauges()),
        ?_assertEqual([], imetrics:get_hist()),
        ?_assertEqual(undefined, imetrics:get_exemplar(foo)),
        ?_assertEqual(#{}, imetrics:get_exemplars())
    ].

-endif.
