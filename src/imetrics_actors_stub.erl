-module(imetrics_actors_stub).

%% No-op implementation of imetrics_actors, selected by compiling with
%% {d, 'IMETRICS_STUB'}. Return values match the shape of the real impl.

-export([new_guild/1, set_gauge/3, unset_gauge/2, dump/2]).

new_guild(_GuildName) -> ok.

set_gauge(_GuildName, _Name, _Value) -> ok.

unset_gauge(_GuildName, _Name) -> ok.

dump(_GuildName, _AggKey) -> [].
