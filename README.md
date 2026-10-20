imetrics
========

imetrics is a lightweight library to allow you to easily instrument your
Erlang applications, and expose the data through a consistent HTTP interface.

Dependencies
------------

- *No R16 support*: We require the "newer" ets functions such as ets:update_counter/4.
- *OTP 25 or later*: tables are created with `{write_concurrency, auto}`, which older releases reject.
- *cowboy*: The `cowboy` application (and its dependencies) must be started before you start `imetrics`
  - `cowboy` depends on `ranch`, `cowlib`, `ssl`, and `crypto` [(see here)](https://ninenines.eu/docs/en/cowboy/2.8/manual/)

Getting started
---------------
Build, launch, and run Erlang and imetrics:

```
rebar3 shell
```

To test imetrics:
```
rebar3 eunit
```

Next, decide if you want a counter or a gauge.

### Counters ###

A counter is an integer that has the nice propertry of being monotonically
increasing while imetrics is running. Why is this nice? Well, if your code is
event-driven, like many Erlang apps, it's very easy to instrument counting such
events. For instance, each time a client connects, we can increment a counter.

```erlang
imetrics:add(client_connections).
```

Counters are also nice because they do not hide local extrema when quantized.
In other words, the general shape of the time series is not hidden by a large
delta-time (low resolution data).

imetrics also supports *tags* on counter metrics, which allow you to bundle many
related counters under one named entry. When used in an http server, we can
count the number of each HTTP response code.

```erlang
imetrics:add(http_responses, #{ code => "200" }),
imetrics:add(http_responses, #{ code => "404" }).
```

### Gauges ###

A gauge is a number, any number. You are in charge of what value it's set to.

```erlang
imetrics:set_gauge(velocity, 50.5).
```

And there are also *tagged* gauges.

```erlang
imetrics:set_gauge(cpu_load_avg, #{ granularity => '1min' }, 0.1),
imetrics:set_gauge(cpu_load_avg, #{ granularity => '5min' }, 3.4).
```

Often, a gauge is a calculation based on some counter over time. For example,
consider tracking `miles_driven` instead of `velocity`. You may find that your
metrics are more flexible down the road.

### Function gauges ###

You can define a gauge with a function. Each time the imetrics HTTP API is called,
your metrics function will be executed to return the gauge's result.

```erlang
imetrics:set_gauge(fn_gauge, fun() -> 1.5 end)
```

Functions have a 5 second timeout.

### Multigauges ###

You can use a function to return multiple tagged gauges. This functionality is called a
"multigauge" in imetrics. When creating the gauge, you provide a list of dimensions,
and then you can return any number of metrics instances with different values for those
dimensions.

```erlang
% single dimension
imetrics:set_multigauge(network_connections, interface, fun () -> [{eth0, 14}, {wlan2, 3}] end),

% multiple dimensions
imetrics:set_multigauge(deliveries, {state, county}, fun () -> [{nc, wake}, 90}, {{nc, orange}, 370}] end),
```

### Actor-based Gauges ###

`imetrics` provides a module to help manage the collection of a gauge metric that occurs
across a set of Erlang processes. Please see the `imetrics_actors` module for documentation
and an example.


### Histograms ###

Histograms are a way to take a continuous set of data and turn it into discrete "buckets," counting the number of elements that occur in a given range. Any value must be able to be accepted by a histogram, so the minimum bucket will count any elements <= to the value stored, and an additional "infinite" bucket will be added on top of any defined buckets to capture any values greater than the maximum value defined. Buckets can be defined manually by providing a list of cutoffs, or generated automatically, with an even distribution, by providing a min, max, and number of buckets. The maximum number of buckets a histogram can have is limited by an environment variable (see below).

```erlang
%manually define bucket cutoffs
imetrics_hist_openmetrics:new(http_response_time, [0, 0.01, 0.05, 0.1, 0.2, 1, 10]),
%automatically generate evenly distributed cutoffs (in this case, 0, 4, 8, ..., 1000)
imetrics_hist_openmetrics:new(packet_size, [0, 1000], 251).
```

Calling `new` again for an existing histogram (the same name and tags) resets its counts to zero. The
reset is not atomic with concurrent observations: an `add` running at the same time may be counted in the
reset histogram or lost.

 Histograms are incremented by indicating a continuous value, which will add one to the bucket in which that value would fall.

 ```erlang
 %using the buckets from above, add one to the 0.05 bucket
imetrics_hist_openmetrics:add(http_response_time, 0.03).
 ```

Histograms can also be *tagged*, just like Counters and Gauges.

```erlang
imetrics_hist_openmetrics:new(http_response_time, #{code => "200"}, [0, 0.01, 0.05, 0.1, 0.2, 1, 10]),
imetrics_hist_openmetrics:new(http_response_time, #{code => "408"}, [0, 0.1, 1, 10, 100]).
```

### Stats ###

Stats is a map tracking stats about a collection of values without
storing the actual values themselves. It tracks number of values, min
value, max value, sum, and squared sum. Designed more for a single bulk
update instead of rapid updates in succession.

```erlang
% retrieve/create named stats
Stats = imetrics:stats(ievent_expiration_jobs),
% for each expiration job, update stats in memory
Stats2 = imetrics_stats:update(ExecutionTime, Stats),
...
% store the result in ets
imetrics:set_stats(ievent_expiration_jobs, Stats2)
```

### Exemplars ###

Exemplars are data points that can be added on top of a chart to provide information on a
specific example of an event to augment the higher-level information offered by metrics more broadly.
Exemplars are associated to a specific counter or histogram bucket (both name and tag, where applicable), and contain, at minimum, a value.
In addition, exemplars can attach labels and timestamps. Labels are user-defined fields of information, provided as quick reference to be displayed.
A trace id is a common label, as it allows linking to detailed information about that trace.
Labels are entered as a map, with label names as keys and label values as the values.
The combined length of label names and values should not exceed 128 UTF-8 characters if intended for use with systems using the [OpenMetrics](https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md#exemplars) specifications.
Finally, exemplars can attach a timestamp. If left undefined, the timestamp will default to the current Erlang system time when `imetrics:set_exemplar` is called.
Timestamps should be in seconds from Unix Epoch, and do not need to be integers.
If exemplars are not updated between queries, additional copies of the exemplar will not be generated,
so creating exemplars on infrequent events will not result in overpopulation of the data set.
Additionally, only the most recent exemplar for a given name and tags is reported: exemplars are
stored per scheduler (see [Concurrency](#concurrency)), and reads return the one with the newest timestamp.
If you pass timestamps yourself, an exemplar set later with an older timestamp is not reported while a newer
one exists on another scheduler. If two exemplars have the same timestamp, either may be reported.
`imetrics:set_exemplar`, when called without a type identifier, or when called with a type of `counter`, does not check to see if the added exemplar is associated with any actual values, and will always return true. `imetrics_hist_openmetrics:set_exemplar`, or `imetrics:set_exemplar` with type `histogram`, works slightly differently, in that the value provided for the exemplar determines which bucket it is associated with in the histogram, and if the function is called for a histogram/tag combination that does not exist, this function will return `{error, {badarg, check_ets}}`.
```erlang
%Minimal set_exemplar
imetrics:set_exemplar(http_responses, 1),
%set_exemplar with all details included
imetrics:set_exemplar(http_responses, #{ code => "404" }, 1, #{traceid => "oHg5SJYRHA0"}, 1684267027.342, counter),

%Setting an exemplar on a histogram:
imetrics_hist_openmetrics:set_exemplar(http_response_times, #{code => "408"}, 15.3),
imetrics:set_exemplar(http_response_times, #{code => "400"}, 23, histogram).
```

### Info ###
The OpenMetrics standard specifies the Info type as a way to expose textual information that is expected to remain constant while a program is running, for example, a version number, a revision control commit, or a compiler version. imetrics supports info metrics with the `set_info` function, which takes two arguments: a Name, and a map of tags to represent the data to be exposed. Info metrics are unique based on Name, so any time `set_info` is called for a given name, the previous entry bearing that name will be overwritten with the new map.

```erlang
imetrics:set_info(server, #{version => "1.7.10", otp => "25.2"}).
```

### Allowed types ###
imetrics will normalize any Name and Tag Value inputs to the add and set_gauge
functions to a binary string. The Name and any Tag Values need to match one of these
guards (See `imetrics:bin/1`):

```erlang
is_atom(V)
is_list(V)
is_binary(V)
is_integer(V)
is_tuple(V) andalso tuple_size(V) =< 8
```

Otherwise, the add and set_gauge functions will return
`{error, {function_clause, check_inputs}}`.

Automatic data collectors
-------------------------

imetrics also includes adapters for common Erlang modules to enable collection of
certain metrics automatically. Further documentation for these modules is available
below:

- **imetrics_cowboy** - An imetrics adapter that collects information about Cowboy response handlers
  and can also annotate metrics with custom user data [_(docs)_](docs/imetrics_cowboy.md)
- **imetrics_cowboy_stream_h** - An older adapter for Cowboy that doesn't collect any custom user data
  _(docs TBD)_
- **imetrics_lager_backend** - A [Lager](https://github.com/erlang-lager/lager) backend that counts log
  events segmented by log level _(docs TBD)_

Retrieving data
---------------

### With Erlang ###

The function `imetrics:get_with_types/0` will return a proplist containing all
the metrics stored by imetrics with their tag dictionaries. Each metric will have
a map of tag permutations, with the value corresponding to each tag permutation.

```erlang
> imetrics:get_with_types().
[{<<"client_connections">>,{counter,[{#{},1}]}},
 {<<"http_responses">>,
  {counter,[{#{code => <<"200">>},1},
            {#{code => <<"404">>},1}]}},
 {<<"cpu_load_avg">>,
  {gauge,[{#{granularity => <<"1min">>},0.1},
          {#{granularity => <<"5min">>},3.4}]}},
 {<<"velocity">>,{gauge,[{#{},50.5}]}}]
```

### With HTTP ###
imetrics starts an HTTP server that returns a plaintext representation of all the
metrics compatible with the [OpenMetrics standard.](https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md)
The URI for accessing this data is `/metrics`.

```
$ curl localhost:8085/metrics
# TYPE client_connections counter
client_connections_total{} 1
# TYPE http_responses counter
http_responses_total{code="200"} 1
http_responses_total{code="404"} 1
# TYPE cpu_load_avg gauge
cpu_load_avg{granularity="1min"} 0.1
cpu_load_avg{granularity="5min"} 3.4
# TYPE velocity gauge
velocity{} 50.5
# EOF
```

## Legacy Behavior ##

For backwards compatibility with previous versions of imetrics, two older interfaces
still work, but they don't support all features. Namely, tagged metrics are unsupported.
(Only the legacy "mapped" metrics, which can store one tag at a time, are supported.
The APIs to create these "mapped" metrics are: `imetrics:add_m/3`, `imetrics:set_gauge_m/3`,
`imetrics:update_gauge_m/3`, and `imetrics:update_counter_dimension/2`.) Use of these
interfaces is strongly discouraged in favor of the more flexible tagging interface.
### With Erlang (Legacy) ###

The function `imetrics:get/0` will return a proplist containing all the metrics
stored by imetrics. Un-mapped metrics have the number value in pos 2 of each
entry. Mapped metrics have a nested proplist containing the metrics for each
associated key.

```erlang
> imetrics:get().
[{<<"client_connections">>,1},
 {<<"velocity">>,50.5},
 {<<"http_responses">>,[{<<"200">>,1},{<<"404">>,1}]},
 {<<"cpu_load_avg">>,[{<<"1min">>,0.1},{<<"5min">>,3.4}]}]
```

All the metric names and mapping keys are normalized as binaries.

### With HTTP (Legacy) ###

imetrics starts a very simple HTTP server that returns an easily parseable
plaintext representation of all the metrics. The URI for accessing this data
is `/imetrics/varz:get`. ~~The somewhat strange URI format is necessary to make
proper use of the built-in HTTP server in Erlang. (See `mod_esi`)~~ (Though
`mod_esi` is no longer used, this URI format is now maintained for backwards
compatibility.)

```
$ curl localhost:8085/imetrics/varz:get
client_connections 1
velocity 50.5
http_responses 200:1 404:1
cpu_load_avg 1min:0.1 5min:3.4
```

Configuration
-------------

| env var                       | default   | desc                                                         |
| ----------------------------- | --------- | ------------------------------------------------------------ |
| `http_server_port`            | `8085`    | Listening port                                               |
| `separator`                   | `<<"_">>` | binary string used to separate tuple elements for Name, Key  |
| `strict_openmetrics_compat`   | `false`   | If set to `true`, metrics will only display on the old HTTP endpoint if they aren't compatible with the OpenMetrics endpoint. Metrics will only display on one endpoint or the other, never both. |
| `openmetrics_exemplar_compat` | `false`   | If set to `true`, counters will display with _total appended to their end, and exemplars will be displayed for counters. When `false`, counters will not get the appended suffix, and exemplars will not be displayed on counters. |
| `hist_max_buckets`            | `64`      | Upper limit on the number of buckets for histograms. Calls to create a histogram with more buckets than this value will fail. |

Concurrency
-----------

Counter and histogram writes are sharded by scheduler: each counter series and
histogram bucket is stored as up to `erlang:system_info(schedulers)` ETS rows,
keyed by the writing scheduler's id, and every read sums the shards. Concurrent
writes to the same metric therefore don't serialize on one ETS row lock.

Because of this, `imetrics:add/1,2,3` and `imetrics:add_m/2,3` return the
calling scheduler's shard value after the increment, not the counter's total,
and `imetrics_hist_openmetrics:add/2,3` returns `{Bucket, ShardCount}`. Read
totals with `imetrics:get_counters/0`, `imetrics:get_with_types/0`,
`imetrics_hist_openmetrics:get_hist/1,2` or the HTTP endpoints. Exemplars are
sharded the same way, and `imetrics:get_exemplar/1` returns the one with the
newest timestamp across the shards; `imetrics:get_exemplars/0` returns every
exemplar that way in one scan, as a map keyed like `get_exemplar/1`. Gauges are
not sharded.

### Trade-offs ###

- **Callers that used the return value of `add`.** A caller that treated the
  result of `add`/`add_m` as the counter's total, or the second element of a
  histogram `add` as the bucket's total, now silently gets a smaller number
  instead of an error. Check for such uses when upgrading.
- **Reads cost more.** Every read sums up to one row per scheduler for each
  series, so the cost of a scrape grows with series × schedulers. With the
  handful of series in the benchmark below the difference was within run-to-run
  noise, but expect it to show on machines with more cores and many series.
  This is negligible for a scraper such as Prometheus. A full read, through
  `get_counters/0`, `get_with_types/0`, `get_hist/0` or `/metrics`, scans each
  table once; `imetrics_hist_openmetrics:get_hist/1,2` and
  `imetrics:get_exemplar/1` instead look up one row per scheduler. Application
  code calling any of these in a hot path pays that cost on every call.
- **Memory grows the same way.** A counter series, histogram bucket or
  exemplar written from many schedulers can take up to one ETS row per
  scheduler instead of one row in total. Measured on OTP 25 (64-bit), a shard
  row takes roughly 200–400 bytes for a counter or histogram bucket, depending
  on how many tags it has, and about 500 bytes for an exemplar with two labels.
  For example, 1,000 counter series written from all 32 schedulers of a machine
  take about 7–13 MB, where unsharded they took well under 1 MB. This matters
  mostly for high-cardinality counters on machines with many cores.
- **Point order.** Points within a counter, in `get_with_types/0` and on
  `/metrics`, now come out sorted by tags instead of in ETS hash order. The
  order was never guaranteed, but output diffs between versions will show
  lines moving.
- **ETS layout.** imetrics' ETS tables are internal. Counter and exemplar keys
  now include the scheduler id, and histogram counts moved to
  `imetrics_hist_counts`, so code reading the tables directly must switch to
  the getters.

### Benchmarks ###

`imetrics_bench` measures write throughput from one process and from one process
per scheduler; see its module doc. Run it with
`rebar3 ct --suite=test/imetrics_bench_SUITE --group=bench`, or call
`imetrics_bench:run().` from a remote shell on a live node. Its PASS/FAIL
criteria only detect writes serializing again; compare the numbers it prints for
smaller changes.

Results from an Apple M1 Max (10 schedulers: 8 performance and 2 efficiency
cores) on OTP 25.2, comparing imetrics before sharding, the first sharded
version (counters and histograms sharded, write_concurrency true) and the
current version (exemplars also sharded, write_concurrency auto). Calls per
second, middle value of 3 runs, 200,000 calls per process:

| case       | before, 1 proc | current, 1 proc | before, 10 procs | first sharded, 10 procs | current, 10 procs | speedup at 10 procs |
|------------|---------------:|----------------:|-----------------:|------------------------:|------------------:|--------------------:|
| `add`      |          4.12M |           4.19M |             147K |                   20.3M |             21.3M |               ~145x |
| `add_dim`  |          2.99M |           3.05M |             145K |                   16.4M |             17.2M |               ~119x |
| `hist`     |          1.53M |           1.57M |              84K |                   7.69M |             7.98M |                ~95x |
| `exemplar` |           729K |            781K |             135K |                    136K |             4.54M |                ~34x |

Single-process throughput is unchanged. Before sharding, 10 processes writing
one metric got 4–18% of the throughput of one process, because they all waited
on the same ETS row lock; sharded, they get 5–6x. Exemplars were still one row
per bucket in the first sharded version. write_concurrency auto made little
difference here, since each case writes one series; with 50 series updated
from every scheduler, a raw ETS microbenchmark ran about twice as fast with
auto as with true.

On the same machine, `imetrics_sparse_write_benchmark` (4 writer processes, each
updating its own series of one counter, with an HTTP scrape every 15 s) measured
1.26M writes/s per writer before sharding, 1.62M in the first sharded version
and 1.62M in the current one (middle of 3 runs). Average scrape time was 1.38,
1.92 and 1.54 ms; each run averages only 9 scrapes of 4 series, so these don't
show a real difference.

Stub build
----------

imetrics can be compiled so that every public call is a no-op: no ETS tables,
worker processes or HTTP server are started; the application and its top-level
supervisor still start, with no children. Stub functions return values with
the same shape as the real ones (e.g. `set_gauge` returns the value, getters return
`[]` except `get_exemplar/1`, which returns `undefined`, and `get_exemplars/0`, which
returns `#{}`, `imetrics_vm_metrics:install/0` does nothing, and `tick`/`tock` still
measure durations), so callers don't need to change.

Enable it by defining `IMETRICS_STUB` when compiling the dependency, from your
project's `rebar.config`:

```erlang
{overrides, [{add, imetrics, [{erl_opts, [{d, 'IMETRICS_STUB'}]}]}]}.
```

To check which implementation a build is using, call `imetrics:impl()`, which
returns `regular` or `stub`.

To run imetrics' own tests against the stub build: `rebar3 as stub eunit`.

Note that `cowboy` is still listed as a required application, so it must remain
available even in a stub build.
