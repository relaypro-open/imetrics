-module(imetrics_actors_guild_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, child_id/1, register_guild/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

child_id(GuildName) ->
    list_to_atom("imetrics_actors_guild_" ++ atom_to_list(GuildName)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

register_guild(GuildName) ->
    Id = child_id(GuildName),
    Spec = #{id => Id,
             start => {imetrics_actors_guild, start_link, [{local, Id}]},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [imetrics_actors_guild]},

    case supervisor:start_child(?MODULE, Spec) of
        {error, already_present} ->
            case supervisor:restart_child(?MODULE, Id) of
                {error, {already_started, Pid}} ->
                    {ok, Pid};
                {ok, Pid} ->
                    {ok, Pid};
                Error ->
                    Error
            end;
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.
