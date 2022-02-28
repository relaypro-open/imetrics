-module(imetrics_hist).

-export([new/3, add/2, dump/1, get/1, get_all/0, get_all_nobin/0, subtract/2,
        approximate_percentiles/2]).

-export([compute_bucket/3, compute_bounding_value/3]).

-define(CATCH_KNOWN_EXC(X),
    try
        X
    catch
        error:badarg ->
            {error, {badarg, check_ets}};
        error:badarith ->
            {error, {badarith, check_inputs}};
        exit:{noproc,{gen_server,call,_}} ->
            {error, {badarg, check_ets}}
    end).

new(Name, Range, NumBuckets) ->
    ?CATCH_KNOWN_EXC(
        begin
            ets:insert(?MODULE, {{Name, metadata}, Range, NumBuckets})
        end
    ).

dump(Name) ->
    ?CATCH_KNOWN_EXC(
        begin
            ets:foldr(
              fun(Element, Acc) ->
                case element(1, Element) of
                    {N, _} when N =:= Name ->
                        [Element|Acc];
                    _ ->
                        Acc
                end
              end, [], ?MODULE)
        end
    ).

get_all() ->
    ?CATCH_KNOWN_EXC(
       begin
           Map = ets:foldl(
             fun({{N, metadata}, [Min, Max], NumBuckets}, A) ->
                     NBin = imetrics_utils:bin(N),
                     NData = maps:get(NBin, A, []),
                     A#{ NBin => [{<<"m">>, Min},
                               {<<"M">>, Max},
                               {<<"n">>, NumBuckets}|NData] };
                ({{N, Bucket}, Count}, A) ->
                     NBin = imetrics_utils:bin(N),
                     NData = maps:get(NBin, A, []),
                     A#{ NBin => [{integer_to_binary(Bucket), Count}|NData] };
                (_, A) ->
                     A
             end, #{}, ?MODULE),
           maps:to_list(Map)
       end
      ).

get_all_nobin() ->
    ?CATCH_KNOWN_EXC(
       begin
           Map = ets:foldl(
             fun({{N, metadata}, [Min, Max], NumBuckets}, A) ->
                     NBin = imetrics_utils:bin(N),
                     NData = maps:get(NBin, A, []),
                     A#{ NBin => [{<<"m">>, Min},
                               {<<"M">>, Max},
                               {<<"n">>, NumBuckets}|NData] };
                ({{N, Bucket}, Count}, A) ->
                     NBin = imetrics_utils:bin(N),
                     NData = maps:get(NBin, A, []),
                     A#{ NBin => [{Bucket, Count}|NData] };
                (_, A) ->
                     A
             end, #{}, ?MODULE),
           maps:to_list(Map)
       end
      ).

get(Name) ->
    ?CATCH_KNOWN_EXC(
        begin
            Data = ets:foldr(
                fun
                    ({{N, metadata}, [Min, Max], NumBuckets}, A) when N =:= Name ->
                        [{<<"m">>, Min},
                         {<<"M">>, Max},
                         {<<"n">>, NumBuckets}|A];
                    ({{N, Bucket}, Count}, A) when N =:= Name ->
                        [{integer_to_binary(Bucket), Count}|A];
                    (_, A) ->
                        A
                end,
                [], ?MODULE),
            BinName = imetrics_utils:bin(Name),
            {BinName, Data}
        end
    ).

add(Name, Value) ->
    ?CATCH_KNOWN_EXC(
        begin
            {Range, NumBuckets} = metadata(Name),
            BucketPos = compute_bucket(Range, NumBuckets, Value),
            Counter = ets:update_counter(?MODULE, {Name, BucketPos}, 1, {{Name, BucketPos}, 0}),
            {BucketPos, Counter}
        end
    ).

subtract(B, A) ->
    lists:filtermap(
      fun ({K, V}) when K =:= <<"m">> orelse
                        K =:= <<"M">> orelse
                        K =:= <<"n">> ->
              {true, {K, V}};
          ({K,V}) ->
              V2 = V - proplists:get_value(K, A, 0),
              if V2 =< 0 ->
                     false;
                 true ->
                     {true, {K, V2}}
              end
      end, B).

% @doc Uses the histogram data to compute approximate percentile
% values. These are approximate because of the discretization of
% the buckets. Except on the extremes, the midpoint of each bucket
% is used as the approximation.
%
% P must be sorted e.g. [0.1, 0.5, 0.9, 0.99]
approximate_percentiles(H, P) ->
    #{sum := Sum,
      min := Min,
      max := Max,
      n := N,
      buckets := Buckets,
      nzmin := NZMin,
      nzmax := NZMax} = describe(H),
    PCounts = [ {float_to_binary(X, [{decimals, 8}, compact]),
                 round(X * Sum)} || X <- P ],
    approximate_percentiles_fold(PCounts, Buckets, Min, Max, N, NZMin, NZMax, 0, []).

approximate_percentiles_fold([], _, _Min, _Max, _N, _NZMin, _NZMax, _RunningSum, Accum) ->
    lists:reverse(Accum);
approximate_percentiles_fold([{PName, 0}|Rest], Buckets, Min, Max, N, NZMin, NZMax, RunningSum, Accum) ->
    Accum2 = case NZMin of
                 undefined ->
                     Accum;
                 _ ->
                     %io:format("pname=~p, pcount=~p, nzmin=~p~n", [PName, 0, NZMin]),
                     [{PName, NZMin}|Accum]
             end,
    approximate_percentiles_fold(Rest, Buckets, Min, Max, N, NZMin, NZMax, RunningSum, Accum2);
approximate_percentiles_fold([{PName, _PCount}|Rest], [], Min, Max, N, NZMin, NZMax, RunningSum, Accum) ->
    Accum2 = case NZMax of
                 undefined ->
                     Accum;
                 _ ->
                     %io:format("pname=~p, pcount=~p, nzmax=~p~n", [PName, _PCount, NZMax]),
                     [{PName, NZMax}|Accum]
             end,
    approximate_percentiles_fold(Rest, [], Min, Max, N, NZMin, NZMax, RunningSum, Accum2);
approximate_percentiles_fold(PCounts=[{PName, PCount}|PRest], Buckets=[{BKey, BVal}|BRest], Min, Max, N, NZMin, NZMax, RunningSum, Accum) ->
    NextSum = RunningSum + BVal,
    if
        RunningSum == PCount orelse
        PCount < NextSum ->
            % this percentile is contained in this bucket.
            % continue to iterate other percentiles without incrementing running sum
            [B0, B1] = compute_bounding_value([Min, Max], N, BKey),
            %io:format("runningsum=~p, nextsum=~p, pname=~p, pcount=~p, b0=~p, b1=~p~n", [RunningSum, NextSum, PName, PCount, B0, B1]),
            Accum2 = [{PName, (B0+B1)/2.0}|Accum],
            approximate_percentiles_fold(PRest, Buckets, Min, Max, N, NZMin, NZMax, RunningSum, Accum2);
        true ->
            approximate_percentiles_fold(PCounts, BRest, Min, Max, N, NZMin, NZMax, NextSum, Accum)
    end.

describe(H) ->
    Map = #{min := Min,
            max := Max,
            n := N,
            buckets := Buckets} = lists:foldl(
      fun({<<"m">>, Min}, Map) ->
              Map#{min => Min};
         ({<<"M">>, Max}, Map) ->
              Map#{max => Max};
         ({<<"n">>, N}, Map) ->
              Map#{n => N};
         ({K, V}, Map=#{sum := Sum, buckets := Buckets}) ->
              Map#{sum => Sum  + V,
                   buckets => [{binary_to_integer(K), V}|Buckets]}
      end, #{sum => 0, buckets => []}, H),
    SortedBuckets = lists:sort(Buckets),
    %io:format("sorted buckets=~p~n", [SortedBuckets]),
    {NonZeroMin, NonZeroMax} = case lists:foldl(
                                      fun({K, V}, {Bm, _BM}) when Bm =:= undefined andalso
                                                                 V > 0 ->
                                              {K, K};
                                         ({K, V}, {Bm, _BM}) when V > 0 ->
                                              {Bm, K};
                                         (_, A) ->
                                              A
                                      end,
                                      {undefined, undefined}, SortedBuckets) of
                                   {undefined, undefined} ->
                                       % all buckets are empty
                                       {undefined, undefined};
                                   {BMin, BMax} ->
                                       [A0,_A1] = compute_bounding_value([Min, Max], N, BMin),
                                       %io:format("nzmin bmin=~p, b0=~p, b1=~p~n", [BMin, A0, _A1]),
                                       [_B0,B1] = compute_bounding_value([Min, Max], N, BMax),
                                       %io:format("nzmax bmax=~p, b0=~p, b1=~p~n", [BMax, _B0, B1]),
                                       {A0/1.0, B1/1.0}
                               end,
    Map#{nzmin => NonZeroMin,
         nzmax => NonZeroMax,
         buckets => SortedBuckets}.

metadata(Name) ->
    case ets:lookup(?MODULE, {Name, metadata}) of
        [{_, Range, NumBuckets}] ->
            {Range, NumBuckets};
        _ ->
            erlang:error(badarg)
    end.

compute_bucket([Min, _Max], _NumBuckets, Value) when Value < Min ->
    0;
compute_bucket([_Min, Max], NumBuckets, Value) when Value >= Max ->
    NumBuckets+1;
compute_bucket([Min, Max], NumBuckets, Value) ->
    StepSize = (Max - Min) / NumBuckets,
    Pos = trunc((Value - Min) / StepSize),
    Pos+1.

compute_bounding_value([_Min, Max], NumBuckets, BucketPos) when BucketPos > NumBuckets ->
    [Max, Max];
compute_bounding_value([Min, _Max], _NumBuckets, BucketPos) when BucketPos < 1 ->
    [Min, Min];
compute_bounding_value([Min, Max], NumBuckets, BucketPos) ->
    StepSize = (Max - Min)  / NumBuckets,
    [((BucketPos-1) * StepSize) + Min,
     (BucketPos * StepSize) + Min].

