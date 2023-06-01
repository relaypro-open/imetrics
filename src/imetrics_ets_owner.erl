-module(imetrics_ets_owner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    ets:new(imetrics_counters, [public, named_table]),
    ets:new(imetrics_gauges, [public, named_table]),
    ets:new(imetrics_map_keys, [public, named_table]),
    ets:new(imetrics_stats, [public, named_table]),
    ets:new(imetrics_data_checkpoint, [public, named_table]),
    ets:new(imetrics_hist, [public, named_table]),
    ets:new(imetrics_hist_openmetrics, [public, named_table]),
    ets:new(imetrics_vm_metrics, [public, named_table]),
    ets:new(imetrics_exemplars, [public, named_table]),

    ExpireCheckpointIntervalHr = application:get_env(imetrics, expire_checkpoint_interval_hr, 1),
    timer:apply_interval(timer:hours(ExpireCheckpointIntervalHr),
                         imetrics, clean_checkpoints, []),

    {ok, #{  }}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

