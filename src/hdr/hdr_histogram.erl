-module(hdr_histogram).

-export([new/3, record_value/2, record_values/3, get_percentiles/3, mean/1, stddev/1, max/1, fold/3]).

-define(IntMax64, 16#FFFFFFFFFFFFFFFF).

-record(hdr_histogram, {
          lowest_trackable_value :: integer(),
          highest_trackable_value :: integer(),
          unit_magnitude :: integer(),
          significant_figures :: integer(),
          sub_bucket_half_count_magnitude :: integer(),
          sub_bucket_half_count :: integer(),
          sub_bucket_mask :: integer(),
          sub_bucket_count :: integer(),
          bucket_count :: integer(),
          min_value=?IntMax64 :: integer(),
          max_value=0 :: integer(),
          normalizing_index_offset=0 :: integer(),
          conversion_ratio=1.0 :: number(),
          counts_len :: integer(),
          total_count=0 :: integer(),
          counts :: [integer()]
         }).

-record(hdr_histogram_bucket_config, {
          lowest_trackable_value :: integer(),
          highest_trackable_value :: integer(),
          unit_magnitude :: integer(),
          significant_figures :: integer(),
          sub_bucket_half_count_magnitude :: integer(),
          sub_bucket_half_count :: integer(),
          sub_bucket_mask :: integer(),
          sub_bucket_count :: integer(),
          bucket_count :: integer(),
          counts_len :: integer()
         }).

-record(hdr_iter, {
          h,
          counts_index=-1 :: integer(),     % raw index into the counts array
          total_count :: integer(),         % snapshot of the length at the time the iterator is created
          count=0 :: integer(),             % value directly from array for the current counts_index
          cumulative_count=0 :: integer(),  % sum of all of the counts up to and including the count at this index
          value=0 :: integer(),             % The current value based on counts_index
          highest_equivalent_value=0 :: integer(),
          lowest_equivalent_value :: integer(),
          median_equivalent_value :: integer(),
          value_iterated_from=0 :: integer(),
          value_iterated_to=0 :: integer(),
          specifics,
          '_next_fp'=fun '_all_values_iter_next'/1
         }).

-record(hdr_iter_percentiles, {
          seen_last_value=false :: boolean(),
          ticks_per_half_distance :: integer(),
          percentile_to_iterate_to=0.0 :: number(),
          percentile=0.0 :: number()
         }).

%% ============================================================================
%% API
%% ============================================================================

new(LowestTrackableValue, HighestTrackableValue, SignificantFigures) ->
    BucketConfig = calculate_bucket_config(LowestTrackableValue, HighestTrackableValue, SignificantFigures),
	io:format("~p~n", [BucketConfig#hdr_histogram_bucket_config.counts_len]),
    Counts = list_to_tuple(lists:duplicate(BucketConfig#hdr_histogram_bucket_config.counts_len, 0)),
    #hdr_histogram{
       lowest_trackable_value=BucketConfig#hdr_histogram_bucket_config.lowest_trackable_value,
       highest_trackable_value=BucketConfig#hdr_histogram_bucket_config.highest_trackable_value,
       unit_magnitude=trunc(BucketConfig#hdr_histogram_bucket_config.unit_magnitude),
       significant_figures=BucketConfig#hdr_histogram_bucket_config.significant_figures,
       sub_bucket_half_count_magnitude=BucketConfig#hdr_histogram_bucket_config.sub_bucket_half_count_magnitude,
       sub_bucket_half_count=BucketConfig#hdr_histogram_bucket_config.sub_bucket_half_count,
       sub_bucket_mask=BucketConfig#hdr_histogram_bucket_config.sub_bucket_mask,
       sub_bucket_count=BucketConfig#hdr_histogram_bucket_config.sub_bucket_count,
       bucket_count=BucketConfig#hdr_histogram_bucket_config.bucket_count,
       counts_len=BucketConfig#hdr_histogram_bucket_config.counts_len,
       counts=Counts
      }.

record_value(H, Value) ->
    record_values(H, Value, 1).

record_values(H, Value, Count) ->
    if Value < 0 ->
           erlang:error(badarg);
       true -> ok
    end,
    CountsIndex = counts_index_for(H, Value),

    if CountsIndex < 0 orelse H#hdr_histogram.counts_len =< CountsIndex ->
           erlang:error(badarg);
       true -> ok
    end,

    H2 = counts_inc_normalised(H, CountsIndex, Count),
    H3 = update_min_max(H2, Value),
    H3.

get_percentiles(H, TicksPerHalfDistance, ValueScale) ->
    Iter = iter_percentile_init(H, TicksPerHalfDistance),
    Percentiles = get_percentiles(Iter, H, ValueScale, []),
    #{percentiles => Percentiles}.

mean(H=#hdr_histogram{total_count=TotalCount}) ->
    Total2 = fold(fun(#hdr_iter{count=Count, value=Value}, Total) ->
                 Total + Count * median_equivalent_value(H, Value)
         end, 0, H),
    Total2 / TotalCount.

stddev(H=#hdr_histogram{total_count=TotalCount}) ->
    Mean = mean(H),
    GeometricDevTotal2 = fold(
                           fun(#hdr_iter{count=Count, value=Value}, GeometricDevTotal) ->
                                   Dev = median_equivalent_value(H, Value) - Mean,
                                   GeometricDevTotal + ((Dev*Dev) * Count)
                           end, 0, H),
    math:sqrt(GeometricDevTotal2 / TotalCount).

max(#hdr_histogram{max_value=0}) -> 0;
max(H=#hdr_histogram{max_value=MaxValue}) ->
    highest_equivalent_value(H, MaxValue).

fold(_Fun, Acc0, #hdr_histogram{total_count=0}) -> Acc0;
fold(Fun, Acc0, H) ->
    fun Fold(Iter, AccIn) ->
        case iter_next(Iter) of
            {true, Iter2=#hdr_iter{count=Count}} when Count /= 0 ->
                Fold(Iter2, Fun(Iter2, AccIn));
            {true, Iter2} ->
                Fold(Iter2, AccIn);
            {false, _} ->
                AccIn
        end
    end(iter_init(H), Acc0).

%% ============================================================================
%% INTERNAL
%% ============================================================================
calculate_bucket_config(LowestTrackableValue, HighestTrackableValue, SignificantFigures) ->

    if LowestTrackableValue < 1 orelse
       SignificantFigures < 1 orelse
       SignificantFigures > 5 orelse
       LowestTrackableValue * 2 > HighestTrackableValue ->
           erlang:error(badarg);
       true ->
           ok
    end,

    LargestValueWithSingleUnitResolution = trunc(2 * math:pow(10, SignificantFigures)),
    SubBucketCountMagnitude = trunc(math:ceil(math:log(LargestValueWithSingleUnitResolution) / math:log(2))),
    SubBucketHalfCountMagnitude = (if SubBucketCountMagnitude > 1 -> SubBucketCountMagnitude; true -> 1 end) - 1,
    SubBucketCount = trunc(math:pow(2, SubBucketHalfCountMagnitude+1)),
    UnitMagnitude = trunc(math:floor(math:log(LowestTrackableValue)/math:log(2))),

    if UnitMagnitude + SubBucketHalfCountMagnitude > 61 ->
           erlang:error(badarg);
       true -> ok
    end,

    BucketCount = buckets_needed_to_cover_value(HighestTrackableValue, SubBucketCount, UnitMagnitude),

    #hdr_histogram_bucket_config{
       lowest_trackable_value=LowestTrackableValue,
       significant_figures=SignificantFigures,
       highest_trackable_value=HighestTrackableValue,
       sub_bucket_half_count_magnitude=SubBucketHalfCountMagnitude,
       unit_magnitude=UnitMagnitude,
       sub_bucket_count=SubBucketCount,
       sub_bucket_half_count=SubBucketCount div 2,
       sub_bucket_mask = (SubBucketCount - 1) bsl UnitMagnitude,
       bucket_count=BucketCount,
       counts_len = (BucketCount+1) * (SubBucketCount div 2)
      }.

buckets_needed_to_cover_value(HighestTrackableValue, SubBucketCount, UnitMagnitude) ->
    SmallestUntrackableValue = SubBucketCount bsl UnitMagnitude,
    buckets_needed_to_cover_value_(HighestTrackableValue, SmallestUntrackableValue, 1).

buckets_needed_to_cover_value_(_Value, SmallestUntrackableValue, BucketsNeeded) when SmallestUntrackableValue  > (?IntMax64 div 2) ->
    BucketsNeeded + 1;
buckets_needed_to_cover_value_(Value, SmallestUntrackableValue, BucketsNeeded) when SmallestUntrackableValue > Value ->
    BucketsNeeded;
buckets_needed_to_cover_value_(Value, SmallestUntrackableValue, BucketsNeeded) ->
    buckets_needed_to_cover_value_(Value, SmallestUntrackableValue bsl 1, BucketsNeeded+1).

counts_index_for(H=#hdr_histogram{unit_magnitude=UnitMagnitude}, Value) ->
    BucketIndex = get_bucket_index(H, Value),
    SubBucketIndex = get_sub_bucket_index(Value, BucketIndex, UnitMagnitude),
    counts_index(H, BucketIndex, SubBucketIndex).

get_bucket_index(#hdr_histogram{sub_bucket_mask=SubBucketMask,
                                  sub_bucket_half_count_magnitude=SubBucketHalfCountMagnitude,
                                  unit_magnitude=UnitMagnitude}, Value) ->
    % smallest power of 2 containing value
    Pow2Ceiling = 64 - count_leading_zeros_64(Value bor SubBucketMask),
    Pow2Ceiling - UnitMagnitude - (SubBucketHalfCountMagnitude+1).

get_sub_bucket_index(Value, BucketIndex, UnitMagnitude) ->
    Value bsr (BucketIndex + UnitMagnitude).

counts_index(#hdr_histogram{sub_bucket_half_count_magnitude=SubBucketHalfCountMagnitude,
                              sub_bucket_half_count=SubBucketHalfCount}, BucketIndex, SubBucketIndex) ->
    % Calculate the index for the first entry in the bucket:
    % (The following is equivalent of ((BucketIndex+1) * SubBucketHalfCount)):
    BucketBaseIndex = (BucketIndex+1) bsl SubBucketHalfCountMagnitude,
    OffsetInBucket = SubBucketIndex - SubBucketHalfCount,
    % The following is the equiavalent of ((SubBucketIndex-SubBucketHalfCount) + BucketBaseIndex;
    BucketBaseIndex + OffsetInBucket.

count_leading_zeros_64(Value) when Value > ?IntMax64 ->
    erlang:error(badarg);
count_leading_zeros_64(Value) ->
    % first iteration splits the range of 0 bits - 64 bits
    count_leading_zeros(Value, 32, 0, 64, 64).

count_leading_zeros(_Value, Shift, _LB, _UB, NumBits) when Shift >= NumBits ->
    0;
count_leading_zeros(_Value, X, X, X, NumBits) ->
    NumBits - X;
count_leading_zeros(Value, Shift, LB, UB, NumBits) when Shift >= LB andalso Shift =< UB ->
    if Value bsr Shift == 0 ->
           count_leading_zeros(Value, (LB+Shift) div 2, LB, Shift, NumBits);
       true ->
           count_leading_zeros(Value, (UB+(Shift+1)) div 2, Shift+1, UB, NumBits)
    end.

counts_inc_normalised(H=#hdr_histogram{counts=Counts, total_count=TotalCount}, Index, Value) ->
    NormalisedIndex = normalize_index(H, Index),
    OrigValue = element(NormalisedIndex+1, Counts),
    H#hdr_histogram{counts=setelement(NormalisedIndex+1, Counts, OrigValue+Value),
                    total_count=TotalCount + Value}.

normalize_index(#hdr_histogram{normalizing_index_offset=NormalizingIndexOffset}, Index) when NormalizingIndexOffset == 0 ->
    Index;
normalize_index(#hdr_histogram{normalizing_index_offset=NormalizingIndexOffset, counts_len=CountsLen}, Index) ->
    NormalizedIndex = Index - NormalizingIndexOffset,
    Adjustment = if NormalizedIndex < 0 -> CountsLen;
                    NormalizedIndex >= CountsLen -> -CountsLen;
                    true -> 0
                 end,
    NormalizedIndex + Adjustment.

update_min_max(H=#hdr_histogram{min_value=MinValue, max_value=MaxValue}, Value) ->
    H#hdr_histogram{
      min_value=(if (Value < MinValue) andalso (Value /= 0) -> Value; true -> MinValue end),
      max_value=(if (Value > MaxValue) -> Value; true -> MaxValue end)
     }.

get_percentiles(Iter, H, ValueScale, Acc) ->
    case iter_next(Iter) of
        {false, _Iter2} ->
            Acc;
        {true, Iter2=#hdr_iter{highest_equivalent_value=HighestEquivalentValue,
                        cumulative_count=CumulativeCount,
                        specifics=Percentiles}} ->
            Value = HighestEquivalentValue / ValueScale,
            Percentile = Percentiles#hdr_iter_percentiles.percentile / 100,
            TotalCount = CumulativeCount,
            InvertedPercentile = if Percentile == 1 -> infinity; true -> (1 / (1-Percentile)) end,

            Acc2 = [#{value => Value,
                      percentile => Percentile,
                      total_count => TotalCount,
                      inverted_percentile => InvertedPercentile}|Acc],

            get_percentiles(Iter2, H, ValueScale, Acc2)
    end.


highest_equivalent_value(H, Value) ->
    next_non_equivalent_value(H, Value)-1.

next_non_equivalent_value(H, Value) ->
    lowest_equivalent_value(H, Value) + size_of_equivalent_value_range(H, Value).

size_of_equivalent_value_range(H=#hdr_histogram{unit_magnitude=UnitMagnitude, sub_bucket_count=SubBucketCount}, Value) ->
    BucketIndex = get_bucket_index(H, Value),
    SubBucketIndex = get_sub_bucket_index(Value, BucketIndex, UnitMagnitude),
    AdjustedBucket = if SubBucketIndex >= SubBucketCount -> BucketIndex + 1; true -> BucketIndex end,
    1 bsl (UnitMagnitude + AdjustedBucket).

lowest_equivalent_value(H=#hdr_histogram{unit_magnitude=UnitMagnitude}, Value) ->
    BucketIndex = get_bucket_index(H, Value),
    SubBucketIndex = get_sub_bucket_index(Value, BucketIndex, UnitMagnitude),
    value_from_index(BucketIndex, SubBucketIndex, UnitMagnitude).

value_from_index(BucketIndex, SubBucketIndex, UnitMagnitude) ->
    SubBucketIndex bsl (BucketIndex + UnitMagnitude).

counts_get_normalised(H, Index) ->
    counts_get_direct(H, normalize_index(H, Index)).

counts_get_direct(#hdr_histogram{counts=Counts}, Index) ->
    element(Index+1, Counts).

value_at_index(#hdr_histogram{sub_bucket_half_count_magnitude=SubBucketHalfCountMagnitude,
                             sub_bucket_half_count=SubBucketHalfCount,
                             unit_magnitude=UnitMagnitude}, Index) ->
    BucketIndex = (Index bsr SubBucketHalfCountMagnitude) - 1,
    SubBucketIndex = (Index band (SubBucketHalfCount - 1)) + SubBucketHalfCount,
    {SubBucketIndex2, BucketIndex2} = if BucketIndex < 0 ->
                                            {SubBucketIndex - SubBucketHalfCount, 0};
                                        true ->
                                            {SubBucketIndex, BucketIndex}
                                     end,
    value_from_index(BucketIndex2, SubBucketIndex2, UnitMagnitude).

median_equivalent_value(H, Value) ->
    lowest_equivalent_value(H, Value) + (size_of_equivalent_value_range(H, Value) bsr 1).

%% ============================================================================
%% ITERATION FUNS
%% ============================================================================
iter_percentile_init(H, TicksPerHalfDistance) ->
    Iter = iter_init(H),
    Percentiles = #hdr_iter_percentiles{ticks_per_half_distance=TicksPerHalfDistance},
    Iter#hdr_iter{specifics=Percentiles,
                  '_next_fp' = fun '_percentile_iter_next'/1}.

iter_init(H=#hdr_histogram{total_count=TotalCount}) ->
    #hdr_iter{h=H, total_count=TotalCount}.

iter_next(Iter=#hdr_iter{'_next_fp'=NextFp}) ->
    NextFp(Iter).

has_buckets(#hdr_iter{counts_index=CountsIndex, h=H}) ->
    #hdr_histogram{counts_len=CountsLen} = H,
    CountsIndex < CountsLen.

has_next(#hdr_iter{cumulative_count=CumulativeCount, total_count=TotalCount}) ->
    CumulativeCount < TotalCount.

move_next(Iter=#hdr_iter{counts_index=CountsIndex}) ->
    Iter2=#hdr_iter{counts_index=CountsIndex2,
                    count=Count,
                    cumulative_count=CumulativeCount,
                    h=H} = Iter#hdr_iter{counts_index=CountsIndex+1},
    case has_buckets(Iter2) of
        false ->
            {false, Iter2};
        true ->
            #hdr_iter{value=Value} = Iter2,
            Iter3 = Iter2#hdr_iter{count=counts_get_normalised(H, CountsIndex2),
                                   cumulative_count=CumulativeCount + Count,
                                   value=value_at_index(H, CountsIndex2),
                                   highest_equivalent_value=highest_equivalent_value(H, Value),
                                   lowest_equivalent_value=lowest_equivalent_value(H, Value),
                                   median_equivalent_value=median_equivalent_value(H, Value)},
            {true, Iter3}
    end.
%% ============================================================================
%% SPECIAL ITERATION FUNS
%% ============================================================================

'_all_values_iter_next'(Iter) ->
    case move_next(Iter) of
        {false, Iter2} ->
            {false, Iter2};
        {true, Iter2} ->
            Iter3 = '_update_iterated_values'(Iter2, Iter2#hdr_iter.value),
            {true, Iter3}
    end.

'_basic_iter_next'(Iter=#hdr_iter{counts_index=CountsIndex, h=H}) ->
    #hdr_histogram{counts_len=CountsLen} = H,
    case {has_next(Iter), CountsIndex >= CountsLen} of
        {false, _} ->
            {false, Iter};
        {_, true} ->
            {false, Iter};
        _ ->
            {_, Iter2} = move_next(Iter),
            {true, Iter2}
    end.

'_percentile_iter_next'(Iter) ->
    case has_next(Iter) of
        false ->
            if (Iter#hdr_iter.specifics)#hdr_iter_percentiles.seen_last_value ->
                   {false, Iter};
               true ->
                   Percentiles = (Iter#hdr_iter.specifics)#hdr_iter_percentiles{seen_last_value=true,
                                                                                percentile=100.0},
                   {true, Iter#hdr_iter{specifics=Percentiles}}
            end;
        true ->
            '_percentile_iter_next_'(Iter)
    end.

'_percentile_iter_next_'(Iter=#hdr_iter{counts_index=-1}) ->
    case '_basic_iter_next'(Iter) of
        {false, Iter2} ->
            {false, Iter2};
        {true, Iter2} ->
            '_do_percentile_iter_next'(Iter2)
    end;
'_percentile_iter_next_'(Iter) ->
    '_do_percentile_iter_next'(Iter).

'_do_percentile_iter_next'(Iter) ->
    #hdr_iter{h=H, cumulative_count=CumulativeCount} = Iter,
    #hdr_histogram{total_count=TotalCount} = H,
    Percentiles2 = Iter#hdr_iter.specifics,
    #hdr_iter_percentiles{percentile_to_iterate_to=PercentileToIterateTo} = Percentiles2,

    CurrentPercentile = (100.0 * CumulativeCount) / TotalCount,

    if Iter#hdr_iter.count /= 0 andalso
       PercentileToIterateTo =< CurrentPercentile ->
           Iter2 = '_update_iterated_values'(Iter, highest_equivalent_value(H, Iter#hdr_iter.value)),
           Percentiles3 = #hdr_iter_percentiles{percentile_to_iterate_to=PercentileToIterateTo2,
                                              ticks_per_half_distance=TicksPerHalfDistance} = Iter2#hdr_iter.specifics,
           Temp = trunc(math:log(100 / (100 - PercentileToIterateTo2)) / math:log(2)) + 1,
           HalfDistance = trunc(math:pow(2, Temp)),
           PercentileReportingTicks = TicksPerHalfDistance * HalfDistance,
           Percentiles4 = Percentiles3#hdr_iter_percentiles{percentile=PercentileToIterateTo2,
                                                            percentile_to_iterate_to=PercentileToIterateTo2 + (100/PercentileReportingTicks)},
           {true, Iter2#hdr_iter{specifics=Percentiles4}};
       true ->
           case '_basic_iter_next'(Iter) of
               {false, Iter2} ->
                   {true, Iter2};
               {true, Iter2} ->
                   '_do_percentile_iter_next'(Iter2)
           end
    end.

'_update_iterated_values'(Iter=#hdr_iter{value_iterated_to=ValueIteratedTo}, NewValueIteratedTo) ->
    Iter#hdr_iter{value_iterated_from=ValueIteratedTo,
                  value_iterated_to=NewValueIteratedTo}.
