%% @doc Write scaling benchmark: checks that the hot write paths (counter add,
%% histogram observe and exemplar) scale with the number of
%% schedulers. It runs in CI through start/0 (see .github/workflows/benchmark.yml).
%%
%% Each case runs for a fixed window (1 second by default) in each of Procs
%% processes at once, three times, keeping the fastest run, with Procs = 1 and
%% Procs = erlang:system_info(schedulers_online). The cases are:
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
%%   CI and the command line: start/0 runs every case, writes
%%   benchmark_results_scaling.json for github-action-benchmark, and crashes if a
%%   pass criterion fails. Use -eval, not -s, so the crash gives a non-zero exit:
%%     erl -noshell -pa $(./rebar3 path --lib)/imetrics/benchmarks -pa $(./rebar3 path) \
%%         -eval 'imetrics_write_scaling_benchmark:start(), init:stop().'
%%
%%   From `rebar3 shell`, or a remote shell on a live node
%%   (e.g. `erl -remsh node@host`, or `bin/myrel remote_console`):
%%     imetrics_write_scaling_benchmark:run().
%%
%% On a live node this adds load to every scheduler for a few seconds.
-module(imetrics_write_scaling_benchmark).

-export([start/0, run/0, run/1, bench/3]).

-define(DURATION_MS, 1000).
-define(BATCH, 100). % calls between clock checks
-define(REPEATS, 3). % measurements per case and process count; the fastest counts

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

%% @doc Entry point for CI: runs every case, writes the results for
%% github-action-benchmark and halts the VM with an error on a failed criterion.
-spec start() -> ok.
start() ->
    application:set_env(imetrics, http_server_port, 0), % automatically configure the port
    #{pass := Pass, cases := Cases} = run(),
    write_results(Cases),
    case Pass of
        true -> ok;
        false -> erlang:error({bench_failed, Cases})
    end.

%% All values are "bigger is better": throughput with every scheduler writing,
%% and how much that throughput scales over one process.
-spec write_results(#{case_name() => case_result()}) -> ok.
write_results(Cases) ->
    Entries = lists:append(
      [[{io_lib:format("~p throughput (all schedulers)", [Case]), "calls/second", Full},
        {io_lib:format("~p scaling over 1 process", [Case]), "x", Scaling}]
       || Case <- [add, add_dim, hist, exemplar],
          #{full := #{calls_per_sec := Full}, scaling := Scaling} <- [maps:get(Case, Cases)]]),
    Json = [io_lib:format("{\"name\": \"~s\", \"unit\": \"~s\", \"value\": ~p}", [Name, Unit, Value])
            || {Name, Unit, Value} <- Entries],
    ok = file:write_file("./benchmark_results_scaling.json",
                         ["[\n", lists:join(",\n", Json), "\n]\n"]).

%% @doc Runs every case for 1 second, prints a table and returns the results.
-spec run() -> #{schedulers := pos_integer(),
                 pass := boolean(),
                 cases := #{case_name() => case_result()}}.
run() ->
    run(?DURATION_MS).

%% @doc Like run/0, with a window of Ms milliseconds per measurement.
-spec run(pos_integer()) -> #{schedulers := pos_integer(),
                                           pass := boolean(),
                              cases := #{case_name() => case_result()}}.
run(Ms) ->
    Started = ensure_started(),
    try
        Schedulers = erlang:system_info(schedulers_online),
        Raw = measure(Schedulers, Ms, ets_cases()),
        Imetrics = measure(Schedulers, Ms, imetrics_cases()),
        Cases = judge(maps:merge(Raw, Imetrics)),
        Pass = lists:all(fun(#{pass := P}) -> P =/= false end, maps:values(Cases)),
        Result = #{schedulers => Schedulers, pass => Pass, cases => Cases},
        print(Result, Ms),
        Result
    after
        [application:stop(imetrics) || Started]
    end.

%% @doc Runs F() in a loop in each of Procs processes for Ms milliseconds.
%% Throughput is the sum of each process's own rate, so one process stalled by
%% the OS costs only its own share, not the whole measurement. With more than one
%% process, each is pinned to its own scheduler and they are released together,
%% so the window measures contention, not how long the VM takes to spread freshly
%% spawned processes over the schedulers.
-spec bench(fun(() -> term()), pos_integer(), pos_integer()) -> result().
bench(F, Procs, Ms) ->
    Self = self(),
    Pids = [spawn_link(fun() ->
                               pin(Id, Procs),
                               Self ! {ready, self()},
                               receive go -> ok end,
                               T0 = erlang:monotonic_time(microsecond),
                               Calls = loop(F, T0 + Ms * 1000, 0),
                               Us = erlang:monotonic_time(microsecond) - T0,
                               Self ! {done, self(), Calls, Us}
                       end)
            || Id <- lists:seq(1, Procs)],
    [receive {ready, P} -> ok end || P <- Pids],
    [P ! go || P <- Pids],
    Done = [receive {done, P, Calls, Us} -> {Calls, Us} end || P <- Pids],
    #{procs => Procs,
      calls => lists:sum([Calls || {Calls, _} <- Done]),
      wall_ms => lists:max([Us || {_, Us} <- Done]) div 1000,
      calls_per_sec => round(lists:sum([Calls * 1000000 / max(1, Us) || {Calls, Us} <- Done]))}.

loop(F, Deadline, Calls) ->
    case erlang:monotonic_time(microsecond) < Deadline of
        true -> batch(F, ?BATCH), loop(F, Deadline, Calls + ?BATCH);
        false -> Calls
    end.

batch(_F, 0) -> ok;
batch(F, N) -> F(), batch(F, N - 1).

pin(Id, Procs) when Procs > 1 ->
    erlang:process_flag(scheduler, Id),
    ok;
pin(_Id, _Procs) ->
    ok.

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
                   fun() -> imetrics:delete_counter(Name, #{}) end}
              end},
        % call shape from the README ("tags on counter metrics")
        {add_dim, fun() ->
                      Name = name(<<"add_dim">>),
                      {fun() -> imetrics:add(Name, #{code => "200"}) end,
                       fun() -> imetrics:delete_counter(Name, #{code => "200"}) end}
                  end},
        % 50 falls in the third of five buckets, so each observe also reads
        % three bucket definitions
        {hist, fun() ->
                   Name = name(<<"hist">>),
                   imetrics_hist_openmetrics:new(Name, [1, 10, 100, 1000]),
                   {fun() -> imetrics_hist_openmetrics:add(Name, 50) end,
                    fun() -> imetrics_hist_openmetrics:delete(Name, #{}) end}
               end},
        % every process sets the exemplar of the same bucket (50 falls in le=100)
        {exemplar, fun() ->
                       Name = name(<<"exemplar">>),
                       imetrics_hist_openmetrics:new(Name, [1, 10, 100, 1000]),
                       Key = #{'__name__' => Name, le => <<"100">>},
                       {fun() -> imetrics_hist_openmetrics:set_exemplar(Name, #{}, 50, #{trace_id => "abc"}) end,
                        fun() ->
                            ets:match_delete(imetrics_exemplars, {{Key, '_'}, '_', '_', '_'}),
                            imetrics_hist_openmetrics:delete(Name, #{})
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
    ets:new(imetrics_write_scaling_benchmark, [public, {write_concurrency, auto}, {decentralized_counters, true}]).

-spec name(binary()) -> binary().
name(Case) ->
    Id = integer_to_binary(erlang:unique_integer([positive])),
    <<"imetrics_write_scaling_benchmark_", Case/binary, "_", Id/binary>>.

-spec measure(pos_integer(), pos_integer(), [bench_case()]) -> #{case_name() => #{single := result(), full := result()}}.
measure(Schedulers, Ms, Cases) ->
    maps:from_list(
      [{Name, #{single => measure_one(Setup, 1, Ms), full => measure_one(Setup, Schedulers, Ms)}}
       || {Name, Setup} <- Cases]).

-spec measure_one(fun(() -> {fun(() -> term()), fun(() -> term())}), pos_integer(), pos_integer()) -> result().
measure_one(Setup, Procs, Ms) ->
    % Best of ?REPEATS: OS noise (another process taking a core, a thread
    % descheduled) can only make a run slower, never faster, so the fastest
    % run is the closest to what the code can do.
    Runs = [measure_once(Setup, Procs, Ms) || _ <- lists:seq(1, ?REPEATS)],
    lists:last(lists:sort(fun(#{calls_per_sec := A}, #{calls_per_sec := B}) -> A =< B end, Runs)).

-spec measure_once(fun(() -> {fun(() -> term()), fun(() -> term())}), pos_integer(), pos_integer()) -> result().
measure_once(Setup, Procs, Ms) ->
    {F, Teardown} = Setup(),
    try
        bench(F, Procs, Ms)
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

print(#{schedulers := S, pass := Pass, cases := Cases}, Ms) ->
    io:format("~nimetrics_write_scaling_benchmark: ~b schedulers, ~b ms per measurement~n", [S, Ms]),
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
