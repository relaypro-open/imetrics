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
    case DataFun() of
        [] ->
            {ok, Req};
        Data ->
            ok = deliver_data(Req, Type, Data),
            {ok, Req}
    end.

% The _Type parameter here allows multiple versions of the deliver_data/3 function
% to deliver different types of data. 'types' here refers to a list of metrics,
% annotated with the "type" of their metric, such as those returned by imetrics:get_with_types/0
deliver_data(Req, _Type = types, Data) ->
    Func = fun deliver_metricfamily/2,
    lists:foreach(fun(Value) -> Func(Req, Value) end, Data).

deliver_metricfamily(Req, {Name, {Type, MetricValue}}) ->
    % as new types of metrics are added to this case statement, they should be added
    % to the exclusion list in varz.erl
    ShouldPrintMetric =
        case {Type, application:get_env(imetrics, strict_openmetrics_compat, false)} of
            {counter, _} ->
                cowboy_req:stream_body(["# TYPE ", Name, " counter\n"], nofin, Req);
            {gauge, _} ->
                cowboy_req:stream_body(["# TYPE ", Name, " gauge\n"], nofin, Req);
            {histogram, _} ->
                cowboy_req:stream_body(["# TYPE ", Name, " histogram\n"], nofin, Req);
            {info, _} ->
                cowboy_req:stream_body(["# TYPE ", Name, " info\n"], nofin, Req);
            {_, false} ->
                cowboy_req:stream_body(["# TYPE ", Name, " unknown\n"], nofin, Req);
            {_, true} ->
                not_ok
        end,
    case {ShouldPrintMetric, MetricValue} of
        {ok, Value} when is_number(Value) ->
            VStr = strnum(Value),
            cowboy_req:stream_body([Name, " ", VStr, "\n"], nofin, Req);
        {ok, Map} when is_map(Map) ->
            MappedValues = maps:to_list(Map),
            deliver_legacy_mapped_metric(Req, Name, MappedValues);
        {ok, MappedValues} when is_list(MappedValues) ->
            case MappedValues of
                [{Tags, _Value} | _] when is_map(Tags) ->
                    deliver_mapped_metric(Req, Type, Name, MappedValues);
                _ ->
                    deliver_legacy_mapped_metric(Req, Name, MappedValues)
            end;
        {not_ok, _} ->
            ok
    end.

get_exemplar_string(Name, Tags) ->
    TagsWithName = imetrics_utils:bin(Tags#{ '__name__' => Name }),
    case imetrics:get_exemplar(TagsWithName) of
        {EValue, Labels, Timestamp} ->
            case is_valid_labels(Labels) of
                true ->
                    EStr = strnum(EValue),
                    TStr = strnum(Timestamp),
                    " # {" ++ create_label_string(Labels) ++ "} " ++ EStr ++ " " ++ TStr;
                false ->
                    ""
            end;
        undefined ->
            ""
    end.

create_label_string(Labels) ->
    Result = maps:fold(fun(Label, Value, Acc) -> ("," ++ binary:bin_to_list(imetrics_utils:bin(Label)) ++ "=\"" ++ binary:bin_to_list(Value) ++ "\"" ++ Acc) end, "", Labels),
    case length(Result) of
        R when R < 3 ->
            Result;
        _ ->
            [_| Result2] = Result,
            Result2
    end.

is_valid_labels(Labels) ->
    maps:fold(fun(Label, Value, Valid) -> Valid and is_valid_label_name(Label) and is_valid_label_value(Value) end, true, Labels).

% valid label names start with a letter and may contain letters and numbers
is_valid_label_name(LabelName) ->
    case re:run(binary:bin_to_list(imetrics_utils:bin(LabelName)), <<"^[a-zA-Z_][a-zA-Z0-9_]*$">>, [{capture, none}]) of
        match -> true;
        _ -> false
    end.

% valid label values do not include double quotes, backslashes, or newlines
% (technically these can be escaped, but writing that logic is more complex)
is_valid_label_value(LabelValue) ->
    case re:run(binary:bin_to_list(LabelValue), <<"^[^\n\"\\\\]*$">>, [{capture, none}]) of
        match -> true;
        _ -> false
    end.

deliver_legacy_mapped_metric(Req, Name, [{Key, Value} | Tail]) ->
    case Key of
        <<"$dim">> ->
            deliver_legacy_mapped_metric(Req, Name, Tail);
        _ ->
            cowboy_req:stream_body(
                [<<Name/binary, "{">>, <<"map_key">>, "=\"", Key, "\"} ", strnum(Value), "\n"], nofin, Req
            ),
            deliver_legacy_mapped_metric(Req, Name, Tail)
    end;
deliver_legacy_mapped_metric(_Req, _Name, []) ->
    ok.

deliver_mapped_metric(Req, Type, Name, [{Tags, Value} | Tail]) ->
    TagString = create_tag_string(Tags),
    case {Type, application:get_env(imetrics, openmetrics_exemplar_compat, false)} of
        {counter, true} ->
            cowboy_req:stream_body([Name, "_total", TagString, " ", strnum(Value), get_exemplar_string(Name, Tags), "\n"], nofin, Req);
        {histogram, _} ->
            cowboy_req:stream_body([Name, "_bucket", TagString, " ", strnum(Value), get_exemplar_string(Name, Tags), "\n"], nofin, Req);
        {info, _} ->
            cowboy_req:stream_body([Name, "_info", TagString, " ", strnum(Value), "\n"], nofin, Req);
        {_, _} ->
            cowboy_req:stream_body([Name, TagString, " ", strnum(Value), "\n"], nofin, Req)
    end,
    
    deliver_mapped_metric(Req, Type, Name, Tail);
deliver_mapped_metric(_Req, _Type, _Name, []) ->
    ok.

create_tag_string(Tags) ->
    TagPairs = lists:foldl(
        fun({TagName, TagValue}, Acc) ->
            [[list_to_binary(atom_to_list(TagName)), "=\"", TagValue, "\""] | Acc]
        end,
        [],
        maps:to_list(Tags)
    ),
    ["{", lists:join(",", lists:reverse(TagPairs)), "}"].

strnum('NaN') -> "NaN";
strnum(N) when is_integer(N) -> integer_to_list(N);
strnum(N) when is_float(N) -> float_to_list(N, [{decimals, 6}, compact]).
