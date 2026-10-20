-module(imetrics).

-include("../include/imetrics.hrl").

%% Public API facade. Calls are delegated to imetrics_impl, or to
%% imetrics_stub when compiled with {d, 'IMETRICS_STUB'}.
-ifdef(IMETRICS_STUB).
-define(IMPL, imetrics_stub).
-else.
-define(IMPL, imetrics_impl).
-endif.

-export([
    add/1,
    add/2,
    add/3,
    set_exemplar/2,
    set_exemplar/3,
    set_exemplar/4,
    set_exemplar/5,
    set_exemplar/6,
    add_m/2,
    add_m/3,
    set_info/2,
    init_counter/1,
    init_counter/2
]).

-export([
    set_gauge/2,
    set_gauge/3,
    set_gauge_m/3,
    set_multigauge/2,
    set_multigauge/3,
    update_gauge/2,
    update_gauge/3,
    update_gauge_m/3
]).

-export([
    set_counter_dimension/2,
    register_slo/2
]).

-export([
    hist/2,
    hist/3,
    hist/4,
    tick/1,
    tick/2,
    tick/3,
    tock/1,
    tock/2,
    tock_as/2,
    tick_s/3,
    tick_s/4,
    tock_s/2,
    tock_as_s/3,
    stop_tick_s/2
]).

-export([
    stats/1,
    set_stats/2
]).

-export([
    get/0,
    get_with_types/0,
    get_counters/0,
    get_gauges/0,
    get_hist/0,
    foldl_slo/3,
    get_slo/2,
    get_exemplar/1,
    get_exemplars/0
]).

-export([
    impl/0
]).

-compile({no_auto_import,[get/0]}).

%% ---------------------------------------------------------
%% Note: consider using counters over gauges where possible.
%% They are usually easier to intrument in code, and they
%% do not hide local extrema when quantized.
%% ---------------------------------------------------------

add(Name) -> ?IMPL:add(Name).
add(Name, TagsOrValue) -> ?IMPL:add(Name, TagsOrValue).
add(Name, Tags, Value) -> ?IMPL:add(Name, Tags, Value).

init_counter(Name) -> ?IMPL:init_counter(Name).
init_counter(Name, Tags) -> ?IMPL:init_counter(Name, Tags).

set_exemplar(A1, A2) -> ?IMPL:set_exemplar(A1, A2).
set_exemplar(A1, A2, A3) -> ?IMPL:set_exemplar(A1, A2, A3).
set_exemplar(A1, A2, A3, A4) -> ?IMPL:set_exemplar(A1, A2, A3, A4).
set_exemplar(A1, A2, A3, A4, A5) -> ?IMPL:set_exemplar(A1, A2, A3, A4, A5).
set_exemplar(Name, Tags, EValue, Labels, Timestamp, Type) ->
    ?IMPL:set_exemplar(Name, Tags, EValue, Labels, Timestamp, Type).

add_m(Name, Key) -> ?IMPL:add_m(Name, Key).
add_m(Name, Key, Value) -> ?IMPL:add_m(Name, Key, Value).

set_gauge(Name, Value) -> ?IMPL:set_gauge(Name, Value).
set_gauge(Name, Tags, Value) -> ?IMPL:set_gauge(Name, Tags, Value).
set_gauge_m(Name, Key, Value) -> ?IMPL:set_gauge_m(Name, Key, Value).

% despite its name, this function actually _increments_ a gauge
update_gauge(Name, Value) -> ?IMPL:update_gauge(Name, Value).
update_gauge(Name, Tags, Value) -> ?IMPL:update_gauge(Name, Tags, Value).
update_gauge_m(Name, Key, Value) -> ?IMPL:update_gauge_m(Name, Key, Value).

set_counter_dimension(Name, Value) -> ?IMPL:set_counter_dimension(Name, Value).

set_info(Name, Tags) -> ?IMPL:set_info(Name, Tags).

register_slo(UIdName, Opts) -> ?IMPL:register_slo(UIdName, Opts).

set_multigauge(Name, Fun) -> ?IMPL:set_multigauge(Name, Fun).
set_multigauge(Name, Dimensions, Fun) -> ?IMPL:set_multigauge(Name, Dimensions, Fun).

stats(Name) -> ?IMPL:stats(Name).
set_stats(Name, Stats) -> ?IMPL:set_stats(Name, Stats).

hist(Name, Buckets) -> ?IMPL:hist(Name, Buckets).
hist(A1, A2, A3) -> ?IMPL:hist(A1, A2, A3).
hist(Name, Tags, Range, NumBuckets) -> ?IMPL:hist(Name, Tags, Range, NumBuckets).

tick(Name) -> ?IMPL:tick(Name).
tick(Name, Unit) -> ?IMPL:tick(Name, Unit).
tick(Name, Tags, Unit) -> ?IMPL:tick(Name, Tags, Unit).

tock(Tick) -> ?IMPL:tock(Tick).
tock(Tick, Fun) -> ?IMPL:tock(Tick, Fun).
tock_as(Tick, NewName) -> ?IMPL:tock_as(Tick, NewName).

%% @doc Tracks tick/tock entries with an opaque map.
%%
%% The caller can choose to either use the returned ref for identification,
%% or the Name parameter. Using the Name parameter can cause collisions if
%% tracking multiple metrics of the same name using the same map state.
%%
tick_s(Ticks, Name, Unit) -> ?IMPL:tick_s(Ticks, Name, Unit).
tick_s(Ticks, Ref, Name, Unit) -> ?IMPL:tick_s(Ticks, Ref, Name, Unit).

%% @doc Stores the tock value for the given ref or Name (See tick_s)
tock_s(Ticks, RefOrName) -> ?IMPL:tock_s(Ticks, RefOrName).
tock_as_s(Ticks, RefOrName, NewName) -> ?IMPL:tock_as_s(Ticks, RefOrName, NewName).
stop_tick_s(Ticks, RefOrName) -> ?IMPL:stop_tick_s(Ticks, RefOrName).

get() -> ?IMPL:get().
get_with_types() -> ?IMPL:get_with_types().
get_counters() -> ?IMPL:get_counters().
get_gauges() -> ?IMPL:get_gauges().
get_exemplar(Key) -> ?IMPL:get_exemplar(Key).
get_exemplars() -> ?IMPL:get_exemplars().
get_hist() -> ?IMPL:get_hist().
foldl_slo(UIdName, F, A) -> ?IMPL:foldl_slo(UIdName, F, A).
get_slo(UIdName, UId) -> ?IMPL:get_slo(UIdName, UId).

%% @doc Returns which implementation this build dispatches to: regular | stub
impl() -> ?IMPL:impl().
