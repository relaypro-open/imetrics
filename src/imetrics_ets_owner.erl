-module(imetrics_ets_owner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, new_ets_table/3, num_ets_tables/0,
        dynamic_tables/0, dynamic_tables_by_module/1]).

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

new_ets_table(Module, Name, Options) ->
    gen_server:call(?SERVER, {new_ets_table, Module,
            Name, Options}, infinity).

num_ets_tables() ->
    gen_server:call(?SERVER, {num_ets_tables}, infinity).

dynamic_tables() ->
    gen_server:call(?SERVER, {dynamic_tables}, infinity).

dynamic_tables_by_module(Module) ->
    gen_server:call(?SERVER, {dynamic_tables_by_module, Module}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    % Static tables
    ets:new(imetrics_counters, [public, named_table]),
    ets:new(imetrics_mapped_counters, [public, named_table]),
    ets:new(imetrics_gauges, [public, named_table]),
    ets:new(imetrics_mapped_gauges, [public, named_table]),
    ets:new(imetrics_stats, [public, named_table]),
    ets:new(imetrics_data_checkpoint, [public, named_table]),

    ExpireCheckpointIntervalHr = application:get_env(imetrics, expire_checkpoint_interval_hr, 1),
    timer:apply_interval(timer:hours(ExpireCheckpointIntervalHr),
                         imetrics, clean_checkpoints, []),

    {ok, #{ n => 4,
            dynamic_tables => [] }}.

handle_call({num_ets_tables}, _From, State=#{n := N}) ->
    {reply, N, State};
handle_call({dynamic_tables}, _From, State=#{dynamic_tables := Tables}) ->
    {reply, Tables, State};
handle_call({dynamic_tables_by_module, Module}, _From, State=#{dynamic_tables := Tables}) ->
    Reply = lists:filter(
      fun(#{module := M}) when Module =:= M ->
              true;
         (_) ->
              false
      end, Tables),
    {reply, Reply, State};
handle_call({new_ets_table, Module, Name, Options}, _From,
            State=#{n := N,
                    dynamic_tables := Tables}) ->
    case ets:info(Name, size) of
        undefined ->
            Res = ets:new(Name, Options),
            {reply, Res, State#{n => N + 1,
                    dynamic_tables => Tables ++ [#{name => Name, module => Module}]}};
        _ ->
            {reply, {error, already_exists}, State}
    end.

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

