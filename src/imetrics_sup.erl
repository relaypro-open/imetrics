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
    ets:new(imetrics_counters, [public, named_table]),
    ets:new(imetrics_mapped_counters, [public, named_table]),
    ets:new(imetrics_gauges, [public, named_table]),
    ets:new(imetrics_mapped_gauges, [public, named_table]),
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => imetrics_http_server,
            start => {imetrics_http_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [imetrics_http_server]}],
    {ok, {SupFlags, ChildSpecs}}.
