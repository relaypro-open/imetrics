imetrics
========

imetrics is a lightweight library to allow you to easily instrument your
Erlang applications, and expose the data through a consistent HTTP interface.

Dependencies
------------

- *No R16 support*: We require the "newer" ets functions such as ets:update_counter/4.
- *cowboy*: The `cowboy` application (and its dependencies) must be started before you start `imetrics`
  - `cowboy` depends on `ranch`, `cowlib`, `ssl`, and `crypto` [(see here)](https://ninenines.eu/docs/en/cowboy/2.8/manual/)

Getting started
---------------
Build, test, launch Erlang:

```
rebar compile
rebar eunit
erl -pa ebin
```

Start the imetrics app and its dependencies:

```erlang
application:ensure_all_started(imetrics).
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
  {counter,[{#{code => <<"404">>},1},
            {#{code => <<"200">>},1}]}},
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
client_connections{} 1
# TYPE http_responses counter
http_responses{code="404"} 1
http_responses{code="200"} 1
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

| env var                     | default   | desc                                                         |
| --------------------------- | --------- | ------------------------------------------------------------ |
| `http_server_port`          | `8085`    | Listening port                                               |
| `separator`                 | `<<"_">>` | binary string used to separate tuple elements for Name, Key  |
| `strict_openmetrics_compat` | `false`   | If set to `true`, metrics will only display on the old HTTP endpoint if they aren't compatible with the OpenMetrics endpoint. Metrics will only display on one endpoint or the other, never both. |

## OpenMetrics conversion

imetrics is currently undergoing an effort to update the format of metrics it
serves over HTTP. The old format is still served at `/imetrics/varz:get`, and it
is a simple plaintext representation of the metrics. Work is ongoing to convert this
representation to the [OpenMetrics standard.](https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md)
This is currently served at `/metrics`, and work continues on the [`feature/openmetrics` branch](https://github.com/relaypro-open/imetrics/tree/feature/openmetrics).
