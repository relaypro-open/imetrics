-module(varz).

-export([init/2, terminate/3]).
-export([get/1, counters/1, gauges/1, hist/1, hist_percentiles/1, slo/1]).

% cowboy functions:
init(Req, State) ->
    [<<"varz">>, FuncNameBinary] = string:split(cowboy_req:binding(func, Req), ":"),
    FuncName = binary_to_atom(FuncNameBinary),
    {ok, Req1} = ?MODULE:FuncName(Req),
    {ok, Req1, State}.
terminate(_Reason, _Req, _State) ->
    ok.

get(Req) ->
    case application:get_env(imetrics, strict_openmetrics_compat, false) of
        false ->
            handle(Req, fun imetrics:get/0);
        true ->
            handle(Req, fun() ->
                [
                    {MetricName, MetricPoints}
                 || {MetricName, {Type, MetricPoints}} <- imetrics:get_with_types(),
                    not ((Type =:= counter) or (Type =:= gauge))
                ]
            end)
    end.

counters(Req) ->
    case application:get_env(imetrics, strict_openmetrics_compat, false) of
        false -> handle(Req, fun imetrics:get_counters/0);
        true -> {ok, Req}
    end.

gauges(Req) ->
    case application:get_env(imetrics, strict_openmetrics_compat, false) of
        false -> handle(Req, fun imetrics:get_gauges/0);
        true -> {ok, Req}
    end.

hist(Req) ->
    handle(Req, fun imetrics:get_hist/0).

hist_percentiles(Req) ->
    % We create a resource key that's the combination of the user agent and the entire
    % query string. Note: this breaks HTTP conventions (namely GET should be idempotent)
    QS = cowboy_req:parse_qs(Req),
    K = qs_str_val(k, QS, ?MODULE_STRING),
    E = qs_e_val(QS),
    Key = resource_key(K, Req),
    handle(
        Req,
        fun() ->
            {Age, Data} = imetrics:get_hist_percentiles(Key, E),
            RespHeaders = #{
                <<"content-type">> => <<"text/plain">>,
                <<"x-imetrics-interval-millis">> => [integer_to_list(Age)]
            },
            {RespHeaders, Data}
        end
    ).

slo(Req) ->
    QS = cowboy_req:parse_qs(Req),
    case qs_atom_val(n, QS, ?MODULE_STRING) of
        UIdName when UIdName =/= ?MODULE_STRING ->
            case qs_str_val(u, QS, ?MODULE_STRING) of
                ?MODULE_STRING ->
                    handle(Req, fun(F, A) ->
                        imetrics:foldl_slo(UIdName, F, A)
                    end);
                UId ->
                    handle(Req, fun() -> imetrics:get_slo(UIdName, UId) end)
            end
    end.

qs_e_val(QS) ->
    Default = 1,
    Value = proplists:get_value(<<"e">>, QS),
    case Value of
        undefined ->
            Default;
        X ->
            binary_to_integer(X)
    end.

qs_atom_val(Name, QS, Default) ->
    Value = proplists:get_value(atom_to_binary(Name, latin1), QS),
    case Value of
        undefined ->
            Default;
        X ->
            binary_to_atom(X)
    end.

qs_str_val(Name, QS, Default) ->
    Value = proplists:get_value(atom_to_binary(Name, latin1), QS),
    case Value of
        undefined ->
            Default;
        X ->
            binary_to_list(X)
    end.

resource_key(K, Req) ->
    UserAgent = cowboy_req:header(<<"http_user_agent">>, Req, <<"default">>),
    iolist_to_binary([
        "imetrics_resource_key_",
        UserAgent,
        "_",
        K
    ]).

handle(Req, DataFun) ->
    try
        case erlang:fun_info(DataFun, arity) of
            {arity, 2} ->
                %% Stream
                Req1 = cowboy_req:set_resp_header(<<"content-type">>, "text/plain", Req),
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
                        Req1 = cowboy_req:set_resp_header(<<"content-type">>, "text/plain", Req),
                        {ok, cowboy_req:reply(200, Req1)};
                    {DataHeaders, Data} ->
                        Req1 = cowboy_req:set_resp_headers(DataHeaders, Req),
                        Req2 = cowboy_req:stream_reply(200, Req1),
                        deliver_data(Req2, Data),
                        cowboy_req:stream_trailers(#{}, Req2),
                        {ok, Req2};
                    Data ->
                        Req1 = cowboy_req:set_resp_header(<<"content-type">>, "text/plain", Req),
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
