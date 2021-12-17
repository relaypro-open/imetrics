imetrics
========

imetrics is a lightweight library to allow you to easily instrument your
Erlang applications, and expose the data through a consistent HTTP interface.

Dependencies
------------

- *No R16 support*: We require the "newer" ets functions such as ets:update_counter/4.
- *inets*: The `inets` application must be started before you start `imetrics`

That's it! imetrics doesn't pull in any external deps.

Getting started
---------------
Build, test, launch Erlang:

```
rebar compile
rebar eunit
erl -pa ebin
```

Start the imetrics app:

```
application:start(inets),
application:start(imetrics).
```

Next, decide if you want a counter or a gauge. 

### Counters ###

A counter is an integer that has the nice propertry of being monotonically
increasing while imetrics is running. Why is this nice? Well, if your code is
event-driven, like many Erlang apps, it's very easy to instrument counting such
events. For instance, each time a client connects, we can increment a counter.

```
imetrics:add(client_connections).
```

Counters are also nice because they do not hide local extrema when quantized.
In other words, the general shape of the time series is not hidden by a large
delta-time (low resolution data).

imetrics also supports *mapped counters*, which allow you to bundle many
related counters under one named entry. When used in an http server, we can
count the number of each HTTP response code.

```
imetrics:add_m(http_responses, "200"),
imetrics:add_m(http_responses, "404").
```

### Gauges ###

A gauge is a number, any number. You are in charge of what value it's set to.

```
imetrics:set_gauge(velocity, 50.5).
```

And there are also *mapped gauges*.

```
imetrics:set_gauge_m(cpu_load_avg, '1min', 0.1),
imetrics:set_gauge_m(cpu_load_avg, '5min', 3.4).
```

Often, a gauge is a calculation based on some counter over time. For example,
consider tracking `miles_driven` instead of `velocity`. You may find that your
metrics are more flexible down the road.

### Stats ###

Stats is a map tracking stats about a collection of values without
storing the actual values themselves. It tracks number of values, min
value, max value, sum, and squared sum. Designed more for a single bulk
update instead of rapid updates in succession.

```
% retrieve/create named stats
Stats = imetrics:stats(ievent_expiration_jobs),
% for each expiration job, update stats in memory
Stats2 = imetrics_stats:update(ExecutionTime, Stats),
...
% store the result in ets
imetrics:set_stats(ievent_expiration_jobs, Stats2)
```

### Allowed types ###
imetrics will normalize any Name and Key input to the add and set_gauge
functions to a binary string. The Name and Key needs to match one of these
guards (See `imetrics:bin/1`):

```
is_atom(V)
is_list(V)
is_binary(V)
is_integer(V)
is_tuple(V) andalso tuple_size(V) =< 8
```

Otherwise, the add and set_gauge functions will return
`{error, {function_clause, check_inputs}}`.

Retrieving data
---------------

### With Erlang ###
The function `imetrics:get/0` will return a proplist containing all the metrics
stored by imetrics. Un-mapped metrics have the number value in pos 2 of each
entry. Mapped metrics have a nested proplist containing the metrics for each
associated key.

```
> imetrics:get().
[{<<"client_connections">>,1},
 {<<"velocity">>,50.5},
 {<<"http_responses">>,[{<<"200">>,1},{<<"404">>,1}]},
 {<<"cpu_load_avg">>,[{<<"1min">>,0.1},{<<"5min">>,3.4}]}]
```

All the metric names and mapping keys are normalized as binaries.

### With HTTP ###
imetrics starts a very simple HTTP server that returns an easily parseable
plaintext representation of all the metrics. The URI for accessing this data
is `/imetrics/varz:get`. The somewhat strange URI format is necessary to make
proper use of the built-in HTTP server in Erlang. (See `mod_esi`)

```
$ curl localhost:8085/imetrics/varz:get
client_connections 1
velocity 50.5
http_responses 200:1 404:1
cpu_load_avg 1min:0.1 5min:3.4
```

Configuration
-------------
| env var              | default                         | desc                                                                            |
| -------------------- | ------------------------------- | ------------------------------------------------------------------------------- |
| `http_server_port`   | `8085`                          | Listening port                                                                  |
| `http_home`          | `/tmp/imetrics`                 | inets http requires a directory for which the http server can have full control |
| `http_server_root`   | `Http_home ++ "/server_root"`   | inets http requirement                                                          |
| `http_document_root` | `Http_home ++ "/document_root"` | inets http requirement                                                          |
| `separator`          | `<<"_">>`                       | binary string used to separate tuple elements for Name, Key                     |

## OpenMetrics conversion

imetrics' current HTTP format is a simple plaintext representation of the metrics.
Work is ongoing to convert this representation to the [OpenMetrics](https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md#protobuf-format)
standard on the [`feature/openmetrics` branch](https://github.com/relaypro-open/imetrics/tree/feature/openmetrics).
