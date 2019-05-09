%% Add this to your lager handlers to track info and above:
%% {imetrics_lager_backend, []}
%%
%% Add this to your lager handlers to track error and above:
%% {imetrics_lager_backend, [{level, error}]}
-module(imetrics_lager_backend).
-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
         code_change/3]).

-define(DEFAULT_LOG_LEVEL, info).

-record(state, {
          level
         }).

-type option() :: {level, lager:log_level()}.

-spec init([option(),...]) -> {ok, #state{}} | {error, {fatal,bad_config}}.
init(Config) when is_list(Config) ->
    Level = proplists:get_value(level, Config, ?DEFAULT_LOG_LEVEL),
    case validate_loglevel(Level) of
        false ->
            {error, {fatal, bad_config}};
        Levels ->
            {ok, #state{level=Levels}}
    end.

handle_call({set_loglevel, Level}, #state{}=State) ->
    case validate_loglevel(Level) of
        false ->
            {ok, {error, bad_loglevel}, State};
        Levels ->
            {ok, ok, State#state{level=Levels}}
    end;
handle_call(get_loglevel, #state{level=Level}=State) ->
    {ok, Level, State};
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Message},
             #state{level=L}=State) ->
    case lager_util:is_loggable(Message, L, imetrics_lager_backend) of
        true ->
            imetrics:add_m(lager, lager_msg:severity(Message)),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

validate_loglevel(Level) ->
    try lager_util:config_to_mask(Level) of
        Levels ->
            Levels
    catch _:_ ->
              false
    end.
