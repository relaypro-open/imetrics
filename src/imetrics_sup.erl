-module(imetrics_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
        #{id => imetrics_ets_owner,
            start => {imetrics_ets_owner, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [imetrics_ets_owner]},
        #{id => imetrics_http_server,
            start => {imetrics_http_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [imetrics_http_server]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
