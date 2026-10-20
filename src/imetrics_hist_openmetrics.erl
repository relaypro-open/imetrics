-module(imetrics_hist_openmetrics).

%% Public API facade. Calls are delegated to imetrics_hist_openmetrics_impl,
%% or to imetrics_hist_openmetrics_stub when compiled with {d, 'IMETRICS_STUB'}.
-ifdef(IMETRICS_STUB).
-define(IMPL, imetrics_hist_openmetrics_stub).
-else.
-define(IMPL, imetrics_hist_openmetrics_impl).
-endif.

-export([new/2, new/3, new/4, add/2, add/3, get_hist/1, get_hist/2, get_all/0]).

-export([set_exemplar/2, set_exemplar/3, set_exemplar/4, set_exemplar/5]).

set_exemplar(Name, EValue) -> ?IMPL:set_exemplar(Name, EValue).
set_exemplar(A1, A2, A3) -> ?IMPL:set_exemplar(A1, A2, A3).
set_exemplar(A1, A2, A3, A4) -> ?IMPL:set_exemplar(A1, A2, A3, A4).
set_exemplar(Name, Tags, EValue, Labels, Timestamp) ->
    ?IMPL:set_exemplar(Name, Tags, EValue, Labels, Timestamp).

add(Name, Value) -> ?IMPL:add(Name, Value).
add(Name, Tags, Value) -> ?IMPL:add(Name, Tags, Value).

new(Name, Buckets) -> ?IMPL:new(Name, Buckets).
new(A1, A2, A3) -> ?IMPL:new(A1, A2, A3).
new(Name, Tags, Range, NumBuckets) -> ?IMPL:new(Name, Tags, Range, NumBuckets).

get_all() -> ?IMPL:get_all().

get_hist(NameOrIdentifier) -> ?IMPL:get_hist(NameOrIdentifier).
get_hist(Name, Tags) -> ?IMPL:get_hist(Name, Tags).
