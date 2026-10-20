-module(imetrics_slo).

%% Public API facade. Calls are delegated to imetrics_slo_impl, or to
%% imetrics_slo_stub when compiled with {d, 'IMETRICS_STUB'}.
-ifdef(IMETRICS_STUB).
-define(IMPL, imetrics_slo_stub).
-else.
-define(IMPL, imetrics_slo_impl).
-endif.

-export([svr_ref/1, uid_name/1,
         get/3, add/3, add/4, put/4, dump/1, dump/2, info/1, foldl_dump/3, remove/2]).

svr_ref(UIdName) -> ?IMPL:svr_ref(UIdName).

uid_name(SvrRef) -> ?IMPL:uid_name(SvrRef).

get(UIdName, UId, Key) -> ?IMPL:get(UIdName, UId, Key).

add(UIdName, UId, Key) -> ?IMPL:add(UIdName, UId, Key).
add(UIdName, UId, Key, Val) -> ?IMPL:add(UIdName, UId, Key, Val).

put(UIdName, UId, Key, Val) -> ?IMPL:put(UIdName, UId, Key, Val).

%% @doc remove/2 is provided for completeness, but will not typically be used. It's only useful
%% if you know a particular UId is no longer relevant and want to manage its lifetime manually.
%% Otherwise, the icount gc has you covered.
remove(UIdName, UId) -> ?IMPL:remove(UIdName, UId).

dump(UIdName) -> ?IMPL:dump(UIdName).
dump(UIdName, UId) -> ?IMPL:dump(UIdName, UId).

info(UIdName) -> ?IMPL:info(UIdName).

foldl_dump(UIdName, F, A) -> ?IMPL:foldl_dump(UIdName, F, A).
