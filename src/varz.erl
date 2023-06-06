-module(varz).

-export([init/2, terminate/3]).
-export([get/1, counters/1, gauges/1, hist/1, slo/1]).

% cowboy functions:
init(Req, State) ->
    [<<"varz">>, FuncNameBinary] = string:split(cowboy_req:binding(func, Req), ":"),
    FuncName = list_to_atom(binary_to_list(FuncNameBinary)),
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

qs_atom_val(Name, QS, Default) ->
    Value = proplists:get_value(list_to_binary(atom_to_list(Name)), QS),
    case Value of
        undefined ->
            Default;
        X ->
            list_to_atom(binary_to_list(X))
    end.

qs_str_val(Name, QS, Default) ->
    Value = proplists:get_value(list_to_binary(atom_to_list(Name)), QS),
    case Value of
        undefined ->
            Default;
        X ->
            binary_to_list(X)
    end.

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
                Dim = serialize_dim_if_exists(Name),
                MIo = serialize_proplist(maps:to_list(Map)),
                cowboy_req:stream_body([<<Name/binary, " ">>, Dim, MIo, "\n"], nofin, Req);
            ({Name, MappedValues}) when is_list(MappedValues) ->
                Dim = serialize_dim_if_exists(Name),
                MIo = serialize_proplist(MappedValues),
                cowboy_req:stream_body([<<Name/binary, " ">>, Dim, MIo, "\n"], nofin, Req)
        end,
        Data
    ).

serialize_proplist(List) ->
    MIo = lists:filtermap(
        fun
            ({_Key = <<"$dim">>, _VStr}) ->
                false;
            ({Key, Value}) when is_number(Value); Value =:= 'NaN' ->
                VStr = strnum(Value),
                {true, [Key, ":", VStr]}
        end,
        List
    ),
    string:join(MIo, " ").

serialize_dim_if_exists(Name) ->
    Results = ets:lookup(imetrics_map_keys, Name),
    case proplists:get_value(Name, Results) of
        undefined -> "";
        Str -> ["$dim:", Str, " "]
    end.

strnum('NaN') -> "NaN";
strnum(N) when is_integer(N) -> integer_to_list(N);
strnum(N) when is_float(N) -> float_to_list(N, [{decimals, 6}, compact]).
