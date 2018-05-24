-module(imetrics_hist).

-export([new/3, add/2, dump/1, get/1, subtract/2,
        approximate_percentiles/2]).

-export([compute_bucket/3]).

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

new(Name, Range, NumBuckets) when is_atom(Name) ->
    ?CATCH_KNOWN_EXC(
        begin
            InternalName = internal_name(Name),
            imetrics_ets_owner:new_ets_table(?MODULE, InternalName,
                [public, named_table]),
            ets:insert(InternalName, {metadata, Range, NumBuckets})
        end
    ).

dump(Name) ->
    ?CATCH_KNOWN_EXC(
        begin
            InternalName = internal_name(Name),
            ets:tab2list(InternalName)
        end
    ).

get(Name) ->
    ?CATCH_KNOWN_EXC(
        begin
            InternalName = internal_name(Name),
            Data = ets:foldr(
                fun
                    ({metadata, [Min, Max], NumBuckets}, A) ->
                        [{<<"m">>, Min},
                         {<<"M">>, Max},
                         {<<"n">>, NumBuckets}|A];
                    ({Bucket, Count}, A) ->
                        [{integer_to_binary(Bucket), Count}|A]
                end,
                [], InternalName),
            BinName = atom_to_binary(external_name(Name), utf8),
            {BinName, Data}
        end
    ).

add(Name, Value) when is_atom(Name) ->
    ?CATCH_KNOWN_EXC(
        begin
            InternalName = internal_name(Name),
            {Range, NumBuckets} = metadata(InternalName),
            BucketPos = compute_bucket(Range, NumBuckets, Value),
            Counter = ets:update_counter(InternalName, BucketPos, 1, {BucketPos, 0}),
            {BucketPos, Counter}
        end
    ).

subtract(B, A) ->
    lists:map(
      fun ({K, V}) when K =:= <<"m">> orelse
                        K =:= <<"M">> orelse
                        K =:= <<"n">> ->
              {K, V};
          ({K,V}) ->
              {K, V - proplists:get_value(K, A, 0)}
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
                                       [A0,A1] = compute_bounding_value([Min, Max], N, BMin),
                                       %io:format("nzmin bmin=~p, b0=~p, b1=~p~n", [BMin, A0, A1]),
                                       [B0,B1] = compute_bounding_value([Min, Max], N, BMax),
                                       %io:format("nzmax bmax=~p, b0=~p, b1=~p~n", [BMax, B0, B1]),
                                       {(A0+A1)/2, (B0+B1)/2}
                               end,
    Map#{nzmin => NonZeroMin,
         nzmax => NonZeroMax,
         buckets => lists:sort(Buckets)}.

metadata(InternalName) ->
    case ets:lookup(InternalName, metadata) of
        [{_, Range, NumBuckets}] ->
            {Range, NumBuckets}
    end.

internal_name(Name) ->
    case atom_to_list(Name) of
        ?MODULE_STRING ++ _ ->
            Name;
        NameStr ->
            list_to_atom(?MODULE_STRING ++ NameStr)
    end.

external_name(Name) ->
    case atom_to_list(Name) of
        ?MODULE_STRING ++ Ex ->
            list_to_atom(Ex);
        _ ->
            Name
    end.

compute_bucket([Min, _Max], _NumBuckets, Value) when Value < Min ->
    0;
compute_bucket([_Min, Max], NumBuckets, Value) when Value >= Max ->
    NumBuckets+1;
compute_bucket([Min, Max], NumBuckets, Value) ->
    StepSize = (Max - Min) / NumBuckets,
    Pos = trunc(Value / StepSize),
    Pos+1.

compute_bounding_value([_Min, Max], NumBuckets, BucketPos) when BucketPos > NumBuckets ->
    [Max, Max];
compute_bounding_value([Min, _Max], _NumBuckets, BucketPos) when BucketPos < 1 ->
    [Min, Min];
compute_bounding_value([Min, Max], NumBuckets, BucketPos) ->
    StepSize = (Max - Min)  / NumBuckets,
    [((BucketPos-1) * StepSize) + Min,
     (BucketPos * StepSize) + Min].

