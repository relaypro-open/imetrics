-module(openmetrics).

-export([init/2, terminate/3]).
-export([get/1]).

% cowboy functions:
init(Req, State) ->
    {ok, Req1} = ?MODULE:get(Req),
    {ok, Req1, State}.
terminate(_Reason, _Req, _State) ->
    ok.

get(Req) ->
    handle(Req, fun imetrics:get/0).

handle(Req, DataFun) ->
    try
        case erlang:fun_info(DataFun, arity) of
            {arity, 2} ->
                %% Stream
                Req1 = cowboy_req:set_resp_header(
                    <<"content-type">>, "application/openmetrics-text", Req
                ),
                Req2 = cowboy_req:stream_reply(200, Req1),
                DataFun(
                    fun(Data, Acc0) ->
                        deliver_data(Req2, [Data]),
                        Acc0 + 1
                    end,
                    0
                ),
                cowboy_req:stream_trailers(#{}, Req2),
                {ok, Req2};
            {arity, 0} ->
                %% Dump
                case DataFun() of
                    [] ->
                        Req1 = cowboy_req:set_resp_header(
                            <<"content-type">>, "application/openmetrics-text", Req
                        ),
                        {ok, cowboy_req:reply(200, Req1)};
                    {DataHeaders, Data} ->
                        Req1 = cowboy_req:set_resp_headers(DataHeaders, Req),
                        Req2 = cowboy_req:stream_reply(200, Req1),
                        deliver_data(Req2, Data),
                        cowboy_req:stream_trailers(#{}, Req2),
                        {ok, Req2};
                    Data ->
                        Req1 = cowboy_req:set_resp_header(
                            <<"content-type">>, "application/openmetrics-text", Req
                        ),
                        Req2 = cowboy_req:stream_reply(200, Req1),
                        deliver_data(Req2, Data),
                        cowboy_req:stream_trailers(#{}, Req2),
                        {ok, Req2}
                end
        end
    catch
        Class:Reason:Stacktrace ->
            io:format("exception ~p:~p~n", [Class, Reason]),
            io:format("~p~n", [Stacktrace]),
            ok
    end.

deliver_data(Req, Data) ->
    lists:foreach(
        fun
            ({Name, Value}) when is_number(Value) ->
                VStr = strnum(Value),
                cowboy_req:stream_body([<<Name/binary, " ">>, VStr, "\n"], nofin, Req);
            ({Name, Map}) when is_map(Map) ->
                MIo = serialize_proplist(maps:to_list(Map)),
                cowboy_req:stream_body([<<Name/binary, " ">>, MIo, "\n"], nofin, Req);
            ({Name, MappedValues}) when is_list(MappedValues) ->
                MIo = serialize_proplist(MappedValues),
                cowboy_req:stream_body([<<Name/binary, " ">>, MIo, "\n"], nofin, Req)
        end,
        Data
    ).

serialize_proplist(List) ->
    MIo = lists:map(
        fun
            ({Key = <<"$dim">>, VStr}) ->
                [Key, ":", VStr];
            ({Key, Value}) when is_number(Value); Value =:= 'NaN' ->
                VStr = strnum(Value),
                [Key, ":", VStr]
        end,
        List
    ),
    string:join(MIo, " ").

strnum('NaN') -> "NaN";
strnum(N) when is_integer(N) -> integer_to_list(N);
strnum(N) when is_float(N) -> float_to_list(N, [{decimals, 6}, compact]).
