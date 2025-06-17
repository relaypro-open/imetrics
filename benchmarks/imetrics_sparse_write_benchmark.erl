%% @doc This benchmark models sparse writes and a single reader against imetrics.
%% 
%% It spawns a number of worker processes that each incrementally updates
%% a different metric, and a single HTTP scraper that periodically pulls metrics
%% from the OpenMetrics endpoint.
-module(imetrics_sparse_write_benchmark).

-export([start/0]).

-define(WorkerProcesses, 128).
-define(WorkerInterval, 10). % milliseconds
-define(HttpInterval, timer:seconds(15)).
-define(WarmPeriod, timer:seconds(20)).
-define(CollectPeriod, timer:seconds(65)).
-define(TimeUnit, microsecond).

start() ->
    application:stop(imetrics),
    application:set_env(imetrics, http_server_port, 0), % automatically configure the port
    application:ensure_all_started(imetrics),

    % create the metrics
    init_metrics(?WorkerProcesses),

    % create an HTTP scrape workload
    inets:start(),
    {ok, Port} = imetrics_http_server:port(),
    HttpScraperPid = init_http_scraper(Port),
    HttpScraperMon = erlang:monitor(process, HttpScraperPid),

    % create a metrics publishing workload
    WorkerPids = start_worker_threads(?WorkerProcesses, []),
    WorkerMon = [ erlang:monitor(process, Pid) || Pid <- WorkerPids ],

    % run the workload in the "warmup" period
    io:format("Warming up (waiting ~p ms)~n", [?WarmPeriod]),
    [ erlang:send(Pid, start) || Pid <- WorkerPids ],
    erlang:send(HttpScraperPid, start),
    timer:sleep(?WarmPeriod),
    
    % start collecting data
    io:format("Gathering stats (waiting ~p ms)~n", [?CollectPeriod]),
    [ erlang:send(Pid, collect) || Pid <- WorkerPids ],
    erlang:send(HttpScraperPid, collect),
    timer:sleep(?CollectPeriod),

    % send the request for the HTTP scraper and workers to send their data
    io:format("Stopping test~n~n", []),
    Self = self(),
    HttpRef = make_ref(),
    erlang:send(HttpScraperPid, {stop, Self, HttpRef}),
    WorkerRefs = [ begin
          Ref = make_ref(),
          erlang:send(Pid, {stop, Self, Ref}),
          Ref
      end || Pid <- WorkerPids ],

    % collect the data
    WriteData = receive_data(WorkerRefs, []),
    [ReadData] = receive_data([HttpRef], []),

    % make sure all processes exited correctly and output our report
    receive_downs([HttpScraperMon|WorkerMon]),
    output_report(WriteData, ReadData).

init_metrics(0) ->
    ok;
init_metrics(Id) ->
    imetrics:add(sparse_write_benchmark, #{id => Id}, 0),
    init_metrics(Id - 1).

init_http_scraper(Port) ->
    spawn_link(fun() -> http_loop(idle, integer_to_list(Port), nocollect, []) end).

http_loop(idle, Port, Collect, Data) ->
    receive
        start ->
            http_loop(start, Port, Collect, Data);
        collect ->
            http_loop(idle, Port, collect, Data)
    end;
http_loop(start, Port, Collect, Data) ->
    Path = "http://127.0.0.1:" ++ Port ++ "/metrics",
    T1 = erlang:monotonic_time(?TimeUnit),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} = httpc:request(get, {Path, []}, [], []),
    T2 = erlang:monotonic_time(?TimeUnit),
    Data2 = case Collect of
        nocollect -> Data;
        collect -> [T2-T1|Data]
    end,
    receive
        {stop, From, Ref} ->
            erlang:send(From, {Ref, Data2});
        collect ->
            http_loop(start, Port, collect, Data2)
    after ?HttpInterval ->
        http_loop(start, Port, Collect, Data2)
    end.

start_worker_threads(0, Acc) ->
    Acc;
start_worker_threads(N, Acc) ->
    Pid = spawn_link(fun() -> worker_loop(idle, N, nocollect, {0, 0}) end),
    start_worker_threads(N-1 , [Pid|Acc]).

worker_loop(idle, Id, Collect, Data) ->
    receive
        start ->
            worker_loop(start, Id, Collect, Data);
        collect ->
            worker_loop(idle, Id, collect, Data)
    end;
worker_loop(start, Id, Collect, Data) ->
    T1 = erlang:monotonic_time(?TimeUnit),
    imetrics:add(sparse_write_benchmark, #{id => Id}),
    T2 = erlang:monotonic_time(?TimeUnit),
    Data2 = case Collect of
        nocollect -> Data;
        collect -> 
            Diff = T2 - T1,
            {Sum, Samples} = Data,
            {Sum+Diff, Samples+1}
    end,
    receive
        {stop, From, Ref} ->
            erlang:send(From, {Ref, Data2});
        collect ->
            worker_loop(start, Id, collect, Data2)
    after ?WorkerInterval ->
        worker_loop(start, Id, Collect, Data2)
    end.

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

output_report(WriteData, ReadData) ->
    {HttpSum, HttpSamples} = lists:foldl(fun(Diff, {Sum, Samples}) -> {Sum+Diff, Samples+1} end, {0, 0}, ReadData),
    {WorkerSum, WorkerSamples} = lists:foldl(fun ({WorkerNSum, WorkerNSamples}, {TotalSum, TotalSamples}) -> {TotalSum+WorkerNSum, TotalSamples+WorkerNSamples} end, {0, 0}, WriteData),

    io:format("== Read benchmark ==~n~n"),

    io:format("HTTP Scrape interval = ~p ms~n", [?HttpInterval]),
    io:format("# scrapes measured   = ~p~n", [HttpSamples]),
    io:format("Response time        = ~p us (avg)~n~n", [HttpSum / HttpSamples]),

    io:format("== Write benchmark ==~n~n"),

    io:format("Processes       = ~p~n", [?WorkerProcesses]),
    io:format("Worker interval = ~p ms~n~n", [?WorkerInterval]),

    io:format("Metric writes   = ~p~n", [WorkerSamples]),
    io:format("Latency         = ~p us (avg)~n~n", [WorkerSum / WorkerSamples]),

    io:format("=====================~n~n"),

    {ok, File} = file:open("./benchmark_results.json", [write]),
    io:format(File, "[~n", []),
    io:format(File, "{\"name\": \"HTTP Response Time (avg)\", \"unit\": \"microseconds\", \"value\": ~p},~n", [HttpSum / HttpSamples]),
    io:format(File, "{\"name\": \"Sparse metrics write latency (avg)\", \"unit\": \"microseconds\", \"value\": ~p}~n", [WorkerSum / WorkerSamples]),
    io:format(File, "]~n", []),
    file:close(File).
