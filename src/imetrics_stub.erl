-module(imetrics_stub).

%% No-op implementation of the imetrics API, selected by compiling with
%% {d, 'IMETRICS_STUB'}. Return values match the shape of imetrics_impl.

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

add(_Name) -> 1.
add(_Name, Tags) when is_map(Tags) -> 1;
add(_Name, Value) -> Value.
add(_Name, _Tags, Value) -> Value.

init_counter(_Name) -> 0.
init_counter(_Name, _Tags) -> 0.

set_exemplar(_, _) -> true.
set_exemplar(_, _, _) -> true.
set_exemplar(_, _, _, _) -> true.
set_exemplar(_, _, _, _, _) -> true.
set_exemplar(_, _, _, _, _, _) -> true.

add_m(_Name, _Key) -> 1.
add_m(_Name, _Key, Value) -> Value.

set_gauge(_Name, Value) -> Value.
set_gauge(_Name, _Tags, Value) -> Value.
set_gauge_m(_Name, _Key, Value) -> Value.

update_gauge(_Name, Value) -> Value.
update_gauge(_Name, _Tags, Value) -> Value.
update_gauge_m(_Name, _Key, Value) -> Value.

%% Like the real implementation: numbers as given, anything else as a binary.
set_counter_dimension(_Name, Value) when is_number(Value) -> Value;
set_counter_dimension(_Name, Value) -> imetrics_utils:bin(Value).

set_info(_Name, _Tags) -> true.

register_slo(_UIdName, _Opts) -> {ok, undefined}.

set_multigauge(_Name, Fun) -> Fun.
set_multigauge(_Name, _Dimensions, Fun) -> Fun.

stats(_Name) -> imetrics_stats:new().
set_stats(_Name, Stats) -> Stats.

hist(_, _) -> true.
hist(_, _, _) -> true.
hist(_, _, _, _) -> true.

%% tick/tock are pure; the real implementation records via the
%% imetrics_hist_openmetrics facade, which is also stubbed.
tick(Name) -> imetrics_impl:tick(Name).
tick(Name, Unit) -> imetrics_impl:tick(Name, Unit).
tick(Name, Tags, Unit) -> imetrics_impl:tick(Name, Tags, Unit).

tock(Tick) -> imetrics_impl:tock(Tick).
tock(Tick, Fun) -> imetrics_impl:tock(Tick, Fun).
tock_as(Tick, NewName) -> imetrics_impl:tock_as(Tick, NewName).

tick_s(Ticks, Name, Unit) -> imetrics_impl:tick_s(Ticks, Name, Unit).
tick_s(Ticks, Ref, Name, Unit) -> imetrics_impl:tick_s(Ticks, Ref, Name, Unit).
tock_s(Ticks, RefOrName) -> imetrics_impl:tock_s(Ticks, RefOrName).
tock_as_s(Ticks, RefOrName, NewName) -> imetrics_impl:tock_as_s(Ticks, RefOrName, NewName).
stop_tick_s(Ticks, RefOrName) -> imetrics_impl:stop_tick_s(Ticks, RefOrName).

get() -> [].
get_with_types() -> [].
get_counters() -> [].
get_gauges() -> [].
get_exemplar(_Key) -> undefined.
get_exemplars() -> #{}.
get_hist() -> [].
foldl_slo(_UIdName, _F, A) -> A.
get_slo(_UIdName, _UId) -> [].

impl() -> stub.
