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
    Req1 = cowboy_req:set_resp_header(
        <<"content-type">>, "application/openmetrics-text", Req
    ),
    Req2 = cowboy_req:stream_reply(200, Req1),
    {ok, Req3} = handle(types, Req2, fun imetrics:get_with_types/0),
    cowboy_req:stream_body("# EOF\n", nofin, Req3),
    {ok, cowboy_req:stream_trailers(#{}, Req3)}.

handle(Type, Req, DataFun) ->
    try
        case DataFun() of
            [] ->
                {ok, Req};
            Data ->
                ok = deliver_data(Req, Type, Data),
                {ok, Req}
        end
    catch
        Class:Reason:Stacktrace ->
            io:format("exception ~p:~p~n", [Class, Reason]),
            io:format("~p~n", [Stacktrace]),
            ok
    end.

deliver_data(Req, Type, Data) ->
    Func =
        case Type of
            types -> fun deliver_metricfamily/2
        end,
    lists:foreach(fun(Value) -> Func(Req, Value) end, Data),
    ok.

deliver_metricfamily(Req, {Type, {Name, MetricValue}}) ->
    case Type of
        counter -> cowboy_req:stream_body(["# TYPE ", Name, " counter\n"], nofin, Req);
        mapped_counter -> cowboy_req:stream_body(["# TYPE ", Name, " counter\n"], nofin, Req);
        gauge -> cowboy_req:stream_body(["# TYPE ", Name, " gauge\n"], nofin, Req);
        mapped_gauge -> cowboy_req:stream_body(["# TYPE ", Name, " gauge\n"], nofin, Req);
        _ -> cowboy_req:stream_body(["# TYPE ", Name, " unknown\n"], nofin, Req)
    end,
    case MetricValue of
        Value when is_number(Value) ->
            VStr = strnum(Value),
            cowboy_req:stream_body([<<Name/binary, " ">>, VStr, "\n"], nofin, Req);
        Map when is_map(Map) ->
            MappedValues = maps:to_list(Map),
            Dim = proplists:get_value(<<"$dim">>, MappedValues, <<"map_key">>),
            deliver_mapped_metric(Req, Name, Dim, MappedValues);
        MappedValues when is_list(MappedValues) ->
            Dim = proplists:get_value(<<"$dim">>, MappedValues, <<"map_key">>),
            deliver_mapped_metric(Req, Name, Dim, MappedValues)
    end,
    ok.

deliver_mapped_metric(Req, Name, Dim, [{Key, Value} | Tail]) ->
    case Key of
        <<"$dim">> ->
            deliver_mapped_metric(Req, Name, Dim, Tail);
        _ ->
            cowboy_req:stream_body(
                [<<Name/binary, "{">>, Dim, "=\"", Key, "\"} ", strnum(Value), "\n"], nofin, Req
            ),
            deliver_mapped_metric(Req, Name, Dim, Tail)
    end;
deliver_mapped_metric(_Req, _Name, _Dim, []) ->
    ok.

strnum('NaN') -> "NaN";
strnum(N) when is_integer(N) -> integer_to_list(N);
strnum(N) when is_float(N) -> float_to_list(N, [{decimals, 6}, compact]).
