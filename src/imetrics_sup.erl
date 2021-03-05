-module(imetrics_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, slo_info/0, register_slo/2]).

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
    EtsOwner = #{id => imetrics_ets_owner,
            start => {imetrics_ets_owner, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [imetrics_ets_owner]},
    HttpSpecs = case application:get_env(imetrics, require_http_server_on_startup) of
                    {ok, false} ->
                        [];
                    _ ->
                        [#{id => imetrics_http_server,
                          start => {imetrics_http_server, start_link, []},
                          restart => permanent,
                          shutdown => 5000,
                          type => worker,
                          modules => [imetrics_http_server]}]
                end,
    ChildSpecs = [EtsOwner] ++ HttpSpecs,
    {ok, {SupFlags, ChildSpecs}}.

slo_info() ->
    UIdNames = lists:filtermap(
                fun({SvrRef, _, worker, [icount]}) -> {true, imetrics_slo:uid_name(SvrRef)};
                   (_) -> false
                end, supervisor:which_children(?MODULE)),
    maps:from_list([{UIdName, imetrics_slo:info(UIdName)} || UIdName <- UIdNames]).

register_slo(UIdName, Opts) ->
    SvrRef = imetrics_slo:svr_ref(UIdName),
    DefOpts = application:get_env(imetrics, slo, #{size => 128,
                                                   opts => [],
                                                   hwm => 32*1024*1024}),
    Opts2 = maps:merge(DefOpts, Opts),
    Spec = #{id => SvrRef,
             start => {icount, start_link, [{local, SvrRef}, Opts2]},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [icount]},
    case supervisor:start_child(?MODULE, Spec) of
        {error, already_present} ->
            case supervisor:restart_child(?MODULE, SvrRef) of
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
