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
    {ok, Req4} = handle(hist, Req3, fun imetrics_hist:get_all_nobin/0),
    cowboy_req:stream_body("# EOF\n", nofin, Req4),
    {ok, cowboy_req:stream_trailers(#{}, Req4)}.

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
            types -> fun deliver_metricfamily/2;
            hist -> fun deliver_hist/2
        end,
    lists:foreach(fun(Value) -> Func(Req, Value) end, Data),
    ok.

% this function gets the "less than or equal" value for a particular bucket
% this special case up top is to allow the last bucket to encompass all values
hist_get_bucket_max(N, {NumBuckets, _}) when N =:= NumBuckets + 1 ->
    "+Inf";
hist_get_bucket_max(N, {NumBuckets, [Min, Max]}) when N =< NumBuckets ->
    [_, BucketMax] = imetrics_hist:compute_bounding_value([Min, Max], NumBuckets, N),
    strnum(BucketMax).

% the basic function to deliver a histogram. it computes the bucket stats, sorts the
% buckets, and passes it to the recursive version
deliver_hist(Req, {Name, Data}) ->
    cowboy_req:stream_body(["# TYPE ", Name, " histogram\n"], nofin, Req),

    % retrieve the bucket statistics
    NumBuckets = proplists:get_value(<<"n">>, Data),
    [Min, Max] = [proplists:get_value(<<"m">>, Data), proplists:get_value(<<"M">>, Data)],

    % sort the list. also, if we're missing a "+Inf" bucket, add it.
    SortedList =
        case proplists:is_defined(NumBuckets + 1, Data) of
            true -> lists:sort(Data);
            false -> lists:sort([{NumBuckets + 1, 0} | Data])
        end,

    deliver_hist(Req, Name, {NumBuckets, [Min, Max]}, {_Acc = 0, _PrevBucket = -1, SortedList}).

% the recursive version, which delivers each bucket and recurses, keeping track
% of an accumulator, since each bucket should include the tally of the previous.
deliver_hist(Req, Name, HistStats, {Acc, PrevBucket, [_Bucket = {N, Value} | Tail]}) when
    is_integer(N)
->
    % if we jumped over more than one bucket, include the bucket immediately before
    % the current bucket to minimize uncertainty
    case (PrevBucket < N - 1) and (Value > 0) of
        true ->
            cowboy_req:stream_body(
                bucket_line(Name, hist_get_bucket_max(N - 1, HistStats), integer_to_binary(Acc)),
                nofin,
                Req
            );
        false ->
            noop
    end,

    % print the current bucket regardless
    cowboy_req:stream_body(
        bucket_line(Name, hist_get_bucket_max(N, HistStats), integer_to_binary(Acc + Value)),
        nofin,
        Req
    ),

    % recurse
    deliver_hist(Req, Name, HistStats, {Acc + Value, N, Tail});
% skip the entries in the list with binary keys, they're stats about the histogram which we've already processed
deliver_hist(Req, Name, HistStats, {Acc, PrevBucket, [_Bucket = {N, _Value} | Tail]}) when
    is_binary(N)
->
    deliver_hist(Req, Name, HistStats, {Acc, PrevBucket, Tail});
% base case, all buckets have been printed.
deliver_hist(_Req, _Name, _HistStats, {_Acc, _PrevBucket, []}) ->
    ok.

bucket_line(Name, BucketMax, Value) ->
    [
        Name,
        "_bucket{le=\"",
        BucketMax,
        "\"} ",
        Value,
        "\n"
    ].

deliver_metricfamily(Req, {Type, {Name, MetricValue}}) ->
    case Type of
        counter -> cowboy_req:stream_body(["# TYPE ", Name, " counter\n"], nofin, Req);
        gauge -> cowboy_req:stream_body(["# TYPE ", Name, " gauge\n"], nofin, Req);
        _ -> cowboy_req:stream_body(["# TYPE ", Name, " unknown\n"], nofin, Req)
    end,
    case MetricValue of
        Value when is_number(Value) ->
            VStr = strnum(Value),
            cowboy_req:stream_body([<<Name/binary, " ">>, VStr, "\n"], nofin, Req);
        Map when is_map(Map) ->
            MIo = serialize_proplist(maps:to_list(Map)),
            cowboy_req:stream_body([<<Name/binary, " ">>, MIo, "\n"], nofin, Req);
        MappedValues when is_list(MappedValues) ->
            MIo = serialize_proplist(MappedValues),
            cowboy_req:stream_body([<<Name/binary, " ">>, MIo, "\n"], nofin, Req)
    end,
    ok.

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
