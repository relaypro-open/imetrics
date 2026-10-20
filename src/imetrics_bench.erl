%% @doc Concurrency benchmark for the hot write paths (counter add and
%% histogram observe). It is not run in CI.
%%
%% Each case runs N = 200,000 calls in each of Procs processes at once, with
%% Procs = 1 and Procs = erlang:system_info(schedulers). The cases are:
%%
%%   add          imetrics:add/1 on one counter, no dimension
%%   add_dim      imetrics:add/2 on one counter with a dimension (#{code => "200"})
%%   hist         imetrics_hist_openmetrics:add/2 on one histogram
%%   exemplar     imetrics_hist_openmetrics:set_exemplar/4 on one histogram bucket
%%   ets_raw      ets:update_counter/4 on one key of a scratch table (contended baseline)
%%   ets_sharded  the same, with the key sharded by scheduler id (target)
%%
%% Pass criteria, as ratios so they don't depend on the machine:
%%   add, add_dim, hist, exemplar: scaling (all-schedulers throughput / 1-process
%%                       throughput) >= half of ets_sharded's scaling in the
%%                       same run, and >= 5x ets_raw with all schedulers
%%   add, add_dim:       with all schedulers, within 5x of ets_sharded
%%
%% These only catch writes serializing again (on one lock, scaling and
%% throughput drop by ~100x); they are deliberately far from normal results,
%% so smaller regressions show up in the numbers, not the verdict. Scaling is
%% judged against ets_sharded rather than a fixed factor because the ceiling
%% depends on the core count and mix (e.g. ets_sharded scales ~4-6x on a
%% 10-core M1 Max with 2 efficiency cores).
%%
%% Metrics use throwaway binary names (no atoms are created) and are deleted
%% afterwards. If imetrics isn't running, it is started and then stopped.
%%
%% How to run:
%%
%%   From rebar3 (common_test group `bench`, excluded from a plain `rebar3 ct`):
%%     rebar3 ct --suite=test/imetrics_bench_SUITE --group=bench
%%
%%   From `rebar3 shell`, or a remote shell on a live node
%%   (e.g. `erl -remsh node@host`, or `bin/myrel remote_console`):
%%     imetrics_bench:run().
%%
%% On a live node this adds load to every scheduler for a few seconds.
-module(imetrics_bench).

-export([run/0, run/1, bench/3]).

-define(N, 200000).

-type result() :: #{procs := pos_integer(),
                    calls := non_neg_integer(),
                    wall_ms := non_neg_integer(),
                    calls_per_sec := non_neg_integer()}.
-type case_name() :: add | add_dim | hist | exemplar | ets_raw | ets_sharded.
-type case_result() :: #{single := result(),
                         full := result(),
                         scaling := float(),
                         vs_ets_raw := float(),
                         vs_ets_sharded := float(),
                         pass := boolean() | na}.

-export_type([result/0, case_name/0, case_result/0]).

%% @doc Runs every case with N = 200,000, prints a table and returns the results.
-spec run() -> #{schedulers := pos_integer(),
                 impl := atom(),
                 pass := boolean(),
                 cases := #{case_name() => case_result()}}.
run() ->
    run(?N).

%% @doc Like run/0, with N calls per process.
-spec run(pos_integer()) -> #{schedulers := pos_integer(),
                              impl := atom(),
                              pass := boolean(),
                              cases := #{case_name() => case_result()}}.
run(N) ->
    Started = ensure_started(),
    try
        Schedulers = erlang:system_info(schedulers),
        Raw = measure(Schedulers, N, ets_cases()),
        Imetrics = measure(Schedulers, N, imetrics_cases()),
        Cases = judge(maps:merge(Raw, Imetrics)),
        Pass = lists:all(fun(#{pass := P}) -> P =/= false end, maps:values(Cases)),
        Result = #{schedulers => Schedulers, impl => imetrics:impl(), pass => Pass, cases => Cases},
        print(Result, N),
        Result
    after
        [application:stop(imetrics) || Started]
    end.

%% @doc Runs F() N times in each of Procs processes at once.
-spec bench(fun(() -> term()), pos_integer(), non_neg_integer()) -> result().
bench(F, Procs, N) ->
    Self = self(),
    T0 = erlang:monotonic_time(microsecond),
    Pids = [spawn_link(fun() -> lists:foreach(fun(_) -> F() end, lists:seq(1, N)),
                                Self ! {done, self()} end)
            || _ <- lists:seq(1, Procs)],
    [receive {done, P} -> ok end || P <- Pids],
    Us = erlang:monotonic_time(microsecond) - T0,
    #{procs => Procs, calls => Procs * N, wall_ms => Us div 1000,
      calls_per_sec => Procs * N * 1000000 div max(1, Us)}.

%% ---------------------------------------------------------------------------

%% Each case is {Name, Setup}; Setup() returns {F, Teardown}. Setup runs
%% before each measurement, so both measurements start from an empty metric.
-type bench_case() :: {case_name(), fun(() -> {fun(() -> term()), fun(() -> term())})}.

-spec imetrics_cases() -> [bench_case()].
imetrics_cases() ->
    [
        {add, fun() ->
                  Name = name(<<"add">>),
                  {fun() -> imetrics:add(Name) end,
                   fun() -> imetrics_impl:delete_counter(Name, #{}) end}
              end},
        % call shape from the README ("tags on counter metrics")
        {add_dim, fun() ->
                      Name = name(<<"add_dim">>),
                      {fun() -> imetrics:add(Name, #{code => "200"}) end,
                       fun() -> imetrics_impl:delete_counter(Name, #{code => "200"}) end}
                  end},
        % 50 falls in the third of five buckets, so each observe also reads
        % three bucket definitions
        {hist, fun() ->
                   Name = name(<<"hist">>),
                   imetrics_hist_openmetrics:new(Name, [1, 10, 100, 1000]),
                   {fun() -> imetrics_hist_openmetrics:add(Name, 50) end,
                    fun() -> imetrics_hist_openmetrics_impl:delete(Name, #{}) end}
               end},
        % every process sets the exemplar of the same bucket (50 falls in le=100)
        {exemplar, fun() ->
                       Name = name(<<"exemplar">>),
                       imetrics_hist_openmetrics:new(Name, [1, 10, 100, 1000]),
                       Key = #{'__name__' => Name, le => <<"100">>},
                       {fun() -> imetrics_hist_openmetrics:set_exemplar(Name, #{}, 50, #{trace_id => "abc"}) end,
                        fun() ->
                            ets:match_delete(imetrics_exemplars, {{Key, '_'}, '_', '_', '_'}),
                            imetrics_hist_openmetrics_impl:delete(Name, #{})
                        end}
                   end}
    ].

-spec ets_cases() -> [bench_case()].
ets_cases() ->
    [
        {ets_raw, fun() ->
                      T = scratch_table(),
                      {fun() -> ets:update_counter(T, key, 1, {key, 0}) end,
                       fun() -> ets:delete(T) end}
                  end},
        {ets_sharded, fun() ->
                          T = scratch_table(),
                          {fun() ->
                               Key = {key, erlang:system_info(scheduler_id)},
                               ets:update_counter(T, Key, 1, {Key, 0})
                           end,
                           fun() -> ets:delete(T) end}
                      end}
    ].

%% same options as the imetrics tables
-spec scratch_table() -> ets:tid().
scratch_table() ->
    ets:new(imetrics_bench, [public, {write_concurrency, auto}, {decentralized_counters, true}]).

-spec name(binary()) -> binary().
name(Case) ->
    Id = integer_to_binary(erlang:unique_integer([positive])),
    <<"imetrics_bench_", Case/binary, "_", Id/binary>>.

-spec measure(pos_integer(), pos_integer(), [bench_case()]) -> #{case_name() => #{single := result(), full := result()}}.
measure(Schedulers, N, Cases) ->
    maps:from_list(
      [{Name, #{single => measure_one(Setup, 1, N), full => measure_one(Setup, Schedulers, N)}}
       || {Name, Setup} <- Cases]).

-spec measure_one(fun(() -> {fun(() -> term()), fun(() -> term())}), pos_integer(), pos_integer()) -> result().
measure_one(Setup, Procs, N) ->
    {F, Teardown} = Setup(),
    try
        bench(F, Procs, N)
    after
        Teardown()
    end.

-spec judge(#{case_name() => #{single := result(), full := result()}}) -> #{case_name() => case_result()}.
judge(Results) ->
    Rate = fun(Case, Which) -> maps:get(calls_per_sec, maps:get(Which, maps:get(Case, Results))) end,
    RawFull = Rate(ets_raw, full),
    ShardedFull = Rate(ets_sharded, full),
    MinScaling = ratio(ShardedFull, Rate(ets_sharded, single)) / 2,
    maps:map(
      fun(Case, R) ->
              Full = Rate(Case, full),
              Scaling = ratio(Full, Rate(Case, single)),
              VsRaw = ratio(Full, RawFull),
              VsSharded = ratio(Full, ShardedFull),
              Pass = case Case of
                         _ when Case =:= add; Case =:= add_dim ->
                             Scaling >= MinScaling andalso VsRaw >= 5 andalso VsSharded >= 1/5;
                         _ when Case =:= hist; Case =:= exemplar ->
                             Scaling >= MinScaling andalso VsRaw >= 5;
                         _ ->
                             na
                     end,
              R#{scaling => Scaling, vs_ets_raw => VsRaw, vs_ets_sharded => VsSharded, pass => Pass}
      end, Results).

-spec ratio(non_neg_integer(), non_neg_integer()) -> float().
ratio(A, B) ->
    A / max(1, B).

print(#{schedulers := S, impl := Impl, pass := Pass, cases := Cases}, N) ->
    io:format("~nimetrics_bench: ~b schedulers, impl=~p, N=~b calls per process~n", [S, Impl, N]),
    io:format("~-12s ~14s ~14s ~9s ~9s ~11s  ~s~n",
              ["case", "1 proc/s", io_lib:format("~b procs/s", [S]), "scaling", "x raw", "x sharded", "result"]),
    lists:foreach(
      fun(Case) ->
              #{single := #{calls_per_sec := One}, full := #{calls_per_sec := Full},
                scaling := Scaling, vs_ets_raw := VsRaw, vs_ets_sharded := VsSharded,
                pass := P} = maps:get(Case, Cases),
              io:format("~-12s ~14b ~14b ~8.2fx ~8.2fx ~10.2fx  ~s~n",
                        [Case, One, Full, Scaling, VsRaw, VsSharded, verdict(P)])
      end, [add, add_dim, hist, exemplar, ets_raw, ets_sharded]),
    #{scaling := ShardedScaling} = maps:get(ets_sharded, Cases),
    io:format("criteria: add/add_dim/hist/exemplar scaling >= ~.2fx (half of ets_sharded's) and x raw >= 5x; "
              "add/add_dim x sharded >= 0.20x~n", [ShardedScaling / 2]),
    io:format("overall: ~s~n~n", [verdict(Pass)]).

verdict(true) -> "PASS";
verdict(false) -> "FAIL";
verdict(na) -> "baseline".

%% Starts imetrics if it isn't running; returns whether it did.
-spec ensure_started() -> boolean().
ensure_started() ->
    case lists:keymember(imetrics, 1, application:which_applications()) of
        true ->
            false;
        false ->
            {ok, _} = application:ensure_all_started(imetrics),
            true
    end.
