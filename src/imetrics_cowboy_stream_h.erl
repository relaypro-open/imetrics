-module(imetrics_cowboy_stream_h).
-behavior(cowboy_stream).

-export([app_init/0]).
-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

-record(state, {next}).

%% 
%% This implementation was modeled from cowboy_compress_h
%%

cowboy_responses_metric() ->
    application:get_env(imetrics, cowboy_responses_metric, cowboy_responses).

cowboy_error_responses_metric() ->
    application:get_env(imetrics, cowboy_error_responses_metric, cowboy_error_responses).

cowboy_active_handlers_metric() ->
    application:get_env(imetrics, cowboy_active_handlers_metric, cowboy_active_handlers).

-spec app_init()
    -> ok.
app_init() ->
    imetrics:set_counter_dimension(cowboy_responses_metric(),
                                  application:get_env(imetrics, cowboy_responses_dimension, code)),
    imetrics:set_counter_dimension(cowboy_error_responses_metric(),
                                  application:get_env(imetrics, cowboy_error_responses_dimension, code)),
    ok.

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
	-> {cowboy_stream:commands(), #state{}}.
init(StreamID, Req, Opts) ->
    imetrics:update_gauge(cowboy_active_handlers_metric(), 1),
	State0 = #state{},
	{Commands0, Next} = cowboy_stream:init(StreamID, Req, Opts),
	fold(Commands0, State0#state{next=Next}).

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
data(StreamID, IsFin, Data, State0=#state{next=Next0}) ->
	{Commands0, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
	fold(Commands0, State0#state{next=Next}).

-spec info(cowboy_stream:streamid(), any(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
info(StreamID, Info, State0=#state{next=Next0}) ->
	{Commands0, Next} = cowboy_stream:info(StreamID, Info, Next0),
	fold(Commands0, State0#state{next=Next}).

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), #state{}) -> any().
terminate(StreamID, Reason, #state{next=Next}) ->
    imetrics:update_gauge(cowboy_active_handlers_metric(), -1),
	cowboy_stream:terminate(StreamID, Reason, Next).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
	cowboy_stream:partial_req(), Resp, cowboy:opts()) -> Resp
	when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
	cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

%% Internal.

fold(Commands, State) ->
	fold(Commands, State, []).

fold([], State, Acc) ->
	{lists:reverse(Acc), State};
fold([Response0={_, _ResponseCode, _Headers=#{<<"imetrics-cowboy-stream-h">> := <<"ignore">>}, _Body}|Tail], State0, Acc) ->
    fold(Tail, State0, [Response0|Acc]);
fold([Response0={response, ResponseCode, _Headers, _Body}|Tail], State0, Acc) ->
    imetrics:add_m(cowboy_responses_metric(), ResponseCode),
    fold(Tail, State0, [Response0|Acc]);
fold([Response0={error_response, ResponseCode, _Headers, _Body}|Tail], State0, Acc) ->
    imetrics:add_m(cowboy_error_responses_metric(), ResponseCode),
    imetrics:add_m(cowboy_responses_metric(), ResponseCode),
    fold(Tail, State0, [Response0|Acc]);
%% Otherwise, we have an unrelated command
fold([Command|Tail], State, Acc) ->
	fold(Tail, State, [Command|Acc]).
