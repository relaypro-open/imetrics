-module(imetrics_hist_openmetrics_stub).

%% No-op implementation of imetrics_hist_openmetrics, selected by compiling
%% with {d, 'IMETRICS_STUB'}. Return values match the shape of the real impl.

-export([new/2, new/3, new/4, add/2, add/3, get_hist/1, get_hist/2, get_all/0]).

-export([set_exemplar/2, set_exemplar/3, set_exemplar/4, set_exemplar/5]).

set_exemplar(_, _) -> true.
set_exemplar(_, _, _) -> true.
set_exemplar(_, _, _, _) -> true.
set_exemplar(_, _, _, _, _) -> true.

add(_Name, _Value) -> {0, 0}.
add(_Name, _Tags, _Value) -> {0, 0}.

new(_, _) -> true.
new(_, _, _) -> true.
new(_, _, _, _) -> true.

get_all() -> [].

get_hist(_) -> [].
get_hist(_, _) -> [].
