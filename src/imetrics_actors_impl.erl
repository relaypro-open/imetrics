-module(imetrics_actors_impl).

-export([new_guild/1, set_gauge/3, unset_gauge/2, dump/2]).

new_guild(GuildName) ->
    imetrics_actors_guild_sup:register_guild(GuildName),
    imetrics:set_multigauge({GuildName, actor_sums}, name, fun() ->
                                                    Svr = imetrics_actors_guild_sup:child_id(GuildName),
                                                    imetrics_actors_guild:dump(Svr, sums)
                                              end),
    imetrics:set_multigauge({GuildName, actor_counts}, name, fun() ->
                                                      Svr = imetrics_actors_guild_sup:child_id(GuildName),
                                                      imetrics_actors_guild:dump(Svr, counts)
                                              end),
    ok.

set_gauge(GuildName, Name, Value) ->
    Actor = self(),
    Svr = imetrics_actors_guild_sup:child_id(GuildName),
    imetrics_actors_guild:set_gauge(Svr, Actor, Name, Value).

unset_gauge(GuildName, Name) ->
    Actor = self(),
    Svr = imetrics_actors_guild_sup:child_id(GuildName),
    imetrics_actors_guild:unset_gauge(Svr, Actor, Name).

dump(GuildName, AggKey) ->
    Svr = imetrics_actors_guild_sup:child_id(GuildName),
    imetrics_actors_guild:dump(Svr, AggKey).
