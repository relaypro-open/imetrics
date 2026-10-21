%% The 'imetrics_actors' module provides a way to create a grouping of processes (actors) that are
%% working together to contribute to a single metric. Each process is responsible
%% for setting its own gauge value, and imetrics will combine the values from all
%% gauges across processes within that group to give a final sum and a final count.
%%
%% For example, suppose your application is responsible for measuring the temperature
%% and number of occupants of all the rooms in your house, and the sensors from each room
%% are governed by an Erlang process (so 1 process per room).
%%
%% (During application startup)
%%     1> imetrics_actors:new_guild(room_sensors).
%%
%% This will initialize the "guild", which is just a too-clever-by-half name for a group of
%% processes. A single actor can belong to many guilds.
%%
%% Then within each process, where you have access to the sensor readings:
%%
%% (executed by the 'Kitchen' actor)
%%     1> imetrics_actors:set_gauge(room_sensors, temperature, 76).
%%     2> imetrics_actors:set_gauge(room_sensors, occupants, 2).
%%
%% (executed by the 'Bedroom' actor. [The temperature sensor is not reporting])
%%     1> imetrics_actors:set_gauge(room_sensors, occupants, 0).
%%
%% (executed by the 'Garage' actor)
%%     1> imetrics_actors:set_gauge(room_sensors, temperature, 89).
%%     2> imetrics_actors:set_gauge(room_sensors, occupants, 1).
%%
%%     5> imetrics:get_gauges().
%%     [{<<"room_sensors_actor_counts">>,
%%       [{<<"$dim">>,<<"name">>},
%%        {<<"occupants">>,3},
%%        {<<"temperature">>,2}]},
%%      {<<"room_sensors_actor_sums">>,
%%       [{<<"$dim">>,<<"name">>},
%%        {<<"occupants">>,3},
%%        {<<"temperature">>,165}]}]
%%
%% room_sensors_actor_counts shows the number of actors that have reported on each metric.
%% room_sensors_actor_sums shows the total sum of each metric over all actors.
%%
%% We can quickly determine that there are 3 people occupying 3 rooms and the average
%% temperature is 165/2 = 82.5.
%%
%% imetrics will automatically track the metric values per-actor. When a process dies, the metrics
%% are removed from the totals.
%%
%% For example, imagine that the Garage actor dies:
%%
%%     6> exit(<0.257.0>, kill).
%%     true
%%     7> imetrics:get_gauges().
%%     [{<<"room_sensors_actor_counts">>,
%%       [{<<"$dim">>,<<"name">>},
%%        {<<"occupants">>,2},
%%        {<<"temperature">>,1}]},
%%      {<<"room_sensors_actor_sums">>,
%%       [{<<"$dim">>,<<"name">>},
%%        {<<"occupants">>,2},
%%        {<<"temperature">>,76}]}]
%%
%% Notice that the values returned by get_gauges/0 have automatically been updated.
%%
%% IMPORTANT: each actor's gauge value is stored internally, and a monitor is created for per process for
%% each guild, so if you have lots of actors, beware of scaling issues!
%%     Using the above example, a house with 200,000 rooms uses ~35 MB of system memory
%%
-module(imetrics_actors).

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
