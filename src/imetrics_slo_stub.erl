-module(imetrics_slo_stub).

%% No-op implementation of imetrics_slo, selected by compiling with
%% {d, 'IMETRICS_STUB'}. Return values match the shape of the real impl.

-export([svr_ref/1, uid_name/1,
         get/3, add/3, add/4, put/4, dump/1, dump/2, info/1, foldl_dump/3, remove/2]).

svr_ref(UIdName) -> imetrics_slo_impl:svr_ref(UIdName).

uid_name(SvrRef) -> imetrics_slo_impl:uid_name(SvrRef).

get(_UIdName, _UId, _Key) -> 0.

add(_UIdName, _UId, _Key) -> ok.
add(_UIdName, _UId, _Key, _Val) -> ok.

put(_UIdName, _UId, _Key, _Val) -> ok.

remove(_UIdName, _UId) -> ok.

dump(_UIdName) -> [].
dump(_UIdName, _UId) -> [].

info(_UIdName) -> #{uids => 0, memory => 0, keys => 0}.

foldl_dump(_UIdName, _F, A) -> A.
