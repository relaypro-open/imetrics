%% @doc This benchmark models heavy contention on a single key.
%%
%% It spawns one worker process per scheduler, and every worker hammers the
%% same counter and the same histogram bucket as fast as it can. This is the
%% opposite of imetrics_sparse_write_benchmark, where each worker has its own
%% series: here all writers want the same ETS row, so throughput shows how well
%% the metric types scale with concurrent writers.
%%
%% When it finishes it checks that no increment was lost: the counter total
%% must equal the number of adds the workers made.
-module(imetrics_contended_write_benchmark).

-export([start/0]).

-define(WorkerProcesses, erlang:system_info(schedulers)).
-define(BatchSize, 100). % writes between checks for a message
-define(WarmPeriod, timer:seconds(5)).
-define(CollectPeriod, timer:seconds(30)).
-define(TimeUnit, microsecond).

-define(Counter, contended_write_benchmark_counter).
-define(Hist, contended_write_benchmark_hist).

start() ->
    application:stop(imetrics),
    application:set_env(imetrics, http_server_port, 0), % automatically configure the port
    application:ensure_all_started(imetrics),

    % create the metrics
    imetrics:add(?Counter, 0),
    imetrics_hist_openmetrics:new(?Hist, [1, 10]),

    % create a metrics publishing workload, all writing the same keys
    WorkerPids = start_worker_threads(?WorkerProcesses, []),
    WorkerMon = [ erlang:monitor(process, Pid) || Pid <- WorkerPids ],

    % run the workload in the "warmup" period
    io:format("Warming up (waiting ~p ms)~n", [?WarmPeriod]),
    [ erlang:send(Pid, start) || Pid <- WorkerPids ],
    timer:sleep(?WarmPeriod),

    % start collecting data
    io:format("Gathering stats (waiting ~p ms)~n", [?CollectPeriod]),
    [ erlang:send(Pid, collect) || Pid <- WorkerPids ],
    timer:sleep(?CollectPeriod),

    % send the request for the workers to send their data
    io:format("Stopping test~n~n", []),
    Self = self(),
    WorkerRefs = [ begin
          Ref = make_ref(),
          erlang:send(Pid, {stop, Self, Ref}),
          Ref
      end || Pid <- WorkerPids ],

    % collect the data
    WriteData = receive_data(WorkerRefs, []),

    % make sure all processes exited correctly and output our report
    receive_downs(WorkerMon),
    output_report(WriteData).

start_worker_threads(0, Acc) ->
    Acc;
start_worker_threads(N, Acc) ->
    Pid = spawn_link(fun() -> worker_loop(idle, nocollect, {0, 0, 0, 0}) end),
    start_worker_threads(N-1 , [Pid|Acc]).

%% State is {StartTime, EndTime, CollectedWrites, TotalWrites}. Writes are
%% counted in pairs: one counter add and one histogram add.
worker_loop(idle, Collect, State) ->
    receive
        start ->
            worker_loop(start, Collect, State);
        collect ->
            worker_loop(idle, collect, State)
    end;
worker_loop(start, Collect, {StartTime, EndTime, Count, Total}) ->
    write_batch(?BatchSize),
    Total2 = Total + ?BatchSize,
    Count2 = case Collect of
        nocollect -> Count;
        collect -> Count + ?BatchSize
    end,
    receive
        {stop, From, Ref} ->
            erlang:send(From, {Ref, {StartTime, erlang:monotonic_time(?TimeUnit), Count2, Total2}});
        collect ->
            worker_loop(start, collect, {erlang:monotonic_time(?TimeUnit), EndTime, Count2, Total2})
    after 0 ->
        worker_loop(start, Collect, {StartTime, EndTime, Count2, Total2})
    end.

write_batch(0) ->
    ok;
write_batch(N) ->
    imetrics:add(?Counter),
    imetrics_hist_openmetrics:add(?Hist, 5),
    write_batch(N - 1).

receive_data([], Acc) ->
    Acc;
receive_data([Ref|Refs], Acc) ->
    receive
        {Ref, DataItem} ->
            receive_data(Refs, [DataItem|Acc])
    end.

receive_downs([]) ->
    ok;
receive_downs([M|Rest]) ->
    receive
        {'DOWN', M, process, _Pid, _Reason} ->
            receive_downs(Rest)
    end.

output_report(WriteData) ->
    PerWorker = [ Count / ((EndTime - StartTime) / 1_000_000) || {StartTime, EndTime, Count, _Total} <- WriteData ],
    TotalThroughput = lists:sum(PerWorker),
    Expected = lists:sum([ Total || {_, _, _, Total} <- WriteData ]),
    Counted = proplists:get_value(atom_to_binary(?Counter), imetrics:get_counters()),
    HistBuckets = imetrics_hist_openmetrics:get_hist(?Hist),
    {_, HistCount} = lists:last(HistBuckets), % the +Inf bucket is cumulative

    io:format("== Write benchmark (single contended key) ==~n~n"),

    io:format("Processes       = ~p~n", [?WorkerProcesses]),
    io:format("Writes per op   = 1 counter add + 1 histogram add~n~n"),

    io:format("Throughput      = ~p ops/s (sum of workers)~n", [TotalThroughput]),
    io:format("Per worker      = ~p ops/s (avg)~n~n", [TotalThroughput / ?WorkerProcesses]),

    io:format("== Integrity ==~n~n"),
    io:format("Expected writes = ~p~n", [Expected]),
    io:format("Counter total   = ~p~n", [Counted]),
    io:format("Histogram total = ~p~n~n", [HistCount]),

    io:format("=====================~n~n"),

    {ok, File} = file:open("./benchmark_results_contended.json", [write]),
    io:format(File, "[~n", []),
    io:format(File, "{\"name\": \"Contended ops throughput (sum of workers)\", \"unit\": \"ops/second\", \"value\": ~p},~n", [TotalThroughput]),
    io:format(File, "{\"name\": \"Contended ops throughput (avg per worker)\", \"unit\": \"ops/second\", \"value\": ~p}~n", [TotalThroughput / ?WorkerProcesses]),
    io:format(File, "]~n", []),
    file:close(File),

    % a lost update means the contended write path is broken
    case {Counted, HistCount} of
        {Expected, Expected} -> ok;
        _ -> erlang:error({lost_updates, #{expected => Expected, counter => Counted, histogram => HistCount}})
    end.
