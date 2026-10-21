-module(imetrics_hist_openmetrics).

-define(TABLE, imetrics_hist_openmetrics).
-define(COUNTS, imetrics_hist_counts).

-export([new/2, new/3, new/4, add/2, add/3, get_hist/1, get_hist/2, get_all/0]).

-export([set_exemplar/2, set_exemplar/3, set_exemplar/4, set_exemplar/5]).

%% Internal, used by the tests.
-export([delete/2]).

%% Tables:
%%   imetrics_hist_openmetrics (read-mostly, written only by new/2,3,4)
%%     names                      -> {names, #{Name => Name}}
%%     {names, Name}              -> {{names, Name}, #{Identifier => Identifier}}
%%     {Identifier, Pos}          -> {{Identifier, Pos}, Cutoff, 0}
%%   imetrics_hist_counts (write-heavy)
%%     {{Identifier, Pos}, Shard} -> {{{Identifier, Pos}, Shard}, Count}
%%
%% Observations are counted in per-scheduler shard rows (Shard = scheduler
%% id, created on first use), so concurrent observes don't serialize on one
%% row lock; readers sum the shards. Counts are kept out of the definitions
%% table so the definition reads every observe does (find_bucket) never share
%% an ETS lock with a write. The third element of a definition row is always
%% 0; counts used to be kept there.

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


set_exemplar(Name, EValue) ->
    set_exemplar(Name, #{}, EValue, #{}, erlang:system_time(millisecond)/1000).
set_exemplar(Name, Tags, EValue) when is_map(Tags) ->
    set_exemplar(Name, Tags, EValue, #{}, erlang:system_time(millisecond)/1000);
set_exemplar(Name, EValue, Labels) when is_map(Labels) ->
    set_exemplar(Name, #{}, EValue, Labels, erlang:system_time(millisecond)/1000);
set_exemplar(Name, EValue, Timestamp) when is_number(Timestamp) ->
    set_exemplar(Name, #{}, EValue, #{}, Timestamp).
set_exemplar(Name, Tags, EValue, Labels) when is_map(Tags), is_map(Labels) ->
    set_exemplar(Name, Tags, EValue, Labels, erlang:system_time(millisecond)/1000);
set_exemplar(Name, Tags, EValue, Timestamp) when is_map(Tags), is_number(Timestamp) ->
    set_exemplar(Name, Tags, EValue, #{}, Timestamp);
set_exemplar(Name, EValue, Labels, Timestamp) when is_map(Labels) ->
    set_exemplar(Name, #{}, EValue, Labels, Timestamp).
    
set_exemplar(Name, Tags, EValue, Labels, Timestamp) ->
    ?CATCH_KNOWN_EXC(
        begin
            BinList = imetrics_utils:bin(Labels),
            TagsWithName = imetrics_utils:bin(Tags#{'__name__' => Name}),
            case ets:lookup(?TABLE, {names, Name}) of
                [{_, _}] ->
                    Bucket = find_bucket(TagsWithName, EValue, 1),
                    [{_, BucketVal, _}] = ets:lookup(?TABLE, {TagsWithName, Bucket}),
                    BucketVal2 = case BucketVal of
                        _ when is_float(BucketVal) -> imetrics_utils:bin(float_to_list(BucketVal, [short]));
                        _ -> imetrics_utils:bin(BucketVal) end,
                    TagsWithLabel = imetrics_utils:bin(TagsWithName#{le => BucketVal2}),
                    % sharded by scheduler like counts; see imetrics:get_exemplar/1
                    ets:insert(imetrics_exemplars, {{TagsWithLabel, erlang:system_info(scheduler_id)}, EValue, BinList, Timestamp});
                [] ->
                    error(badarg)
            end
        end
    ).


add(Name, Value) ->
    add(Name, #{}, Value).
add(Name, Tags, Value) ->
    ?CATCH_KNOWN_EXC(
        begin
            TagsWithName = imetrics_utils:bin(Tags#{'__name__' => Name}),
            Bucket = find_bucket(TagsWithName, Value, 1),
            Key = {{TagsWithName, Bucket}, erlang:system_info(scheduler_id)},
            % the count is this scheduler's shard, not the bucket total
            {Bucket, ets:update_counter(?COUNTS, Key, 1, {Key, 0})}
        end
    ).

new(Name, Buckets) ->
    new(Name, #{}, Buckets).
new(Name, Tags, Buckets) when is_map(Tags) ->
    ?CATCH_KNOWN_EXC(
        begin
            MaxBuckets = application:get_env(imetrics, hist_max_buckets, 64),
            case length(Buckets) of
                X when X > MaxBuckets ->
                    error(badarg);
                _ ->
                    TagsWithName = imetrics_utils:bin(Tags#{ '__name__' => Name}),
                    make_buckets(TagsWithName, 1, Buckets),
                    store_name(Name, Tags)
            end
        end  
    );

new(Name, Range, NumBuckets) ->
    new(Name, #{}, Range, NumBuckets).
new(Name, Tags, Range, NumBuckets) ->
    ?CATCH_KNOWN_EXC(
        begin
            TagsWithName = imetrics_utils:bin(Tags#{ '__name__' => Name}),
            [Min, Max] = Range,
            Delta = Max-Min,
            MaxBuckets = application:get_env(imetrics, hist_max_buckets, 64),
            case {NumBuckets, Delta, NumBuckets} of
                {_, _, X} when X > MaxBuckets ->
                    error(badarg);
                {0, _, _} ->
                    make_buckets(TagsWithName, 1, []),
                    store_name(Name, Tags);
                {1, 0, _} ->
                    make_buckets(TagsWithName, 1, [Min]),
                    store_name(Name, Tags);
                {_, 0, _} ->
                    error(badarith);
                {_, _, _} ->
                    Diff = Delta/(NumBuckets - 1),
                    make_buckets(TagsWithName, 1, calculate_buckets(NumBuckets, Max, Min, Diff, [])),
                    store_name(Name, Tags)
            end
        end  
    ).

get_all() ->
    case ets:lookup(?TABLE, names) of
        [{names, Names}] ->
            % one scan of the shard rows instead of one lookup per bucket per scheduler
            Counts = bucket_totals(),
            maps:fold(
                fun(_, Name, Acc) -> 
                    Group = case ets:lookup(?TABLE, {names, Name}) of
                        [{_, Identifiers}] -> 
                            maps:fold(
                                fun(_, Identifier, Acc2) -> 
                                    get_hist(Identifier, 1, 0, [], Counts) ++ Acc2
                                end,
                                [], Identifiers);
                        [] ->
                            get_hist(Name)
                    end,
                    [{imetrics_utils:bin(Name), Group} | Acc]
                end,
                [], Names);
        [] ->
            []
    end.

get_hist(Identifier) when is_map(Identifier) ->
    get_hist(Identifier, 1, 0, []);
get_hist(Name) ->
    get_hist(Name, #{}).
get_hist(Name, Tags) ->
    get_hist(imetrics_utils:bin(Tags#{'__name__' => Name}), 1, 0, []).
get_hist(Identifier, BucketPos, Count, Acc) ->
    get_hist(Identifier, BucketPos, Count, Acc, lookup).
%% Counts is either `lookup' (sum each bucket's shards with lookups) or a map
%% of bucket totals from bucket_totals/0.
get_hist(Identifier, BucketPos, Count, Acc, Counts) ->
    case ets:lookup(?TABLE, {Identifier, BucketPos}) of
        [{_, Cutoff, _}] ->
            Value = bucket_total({Identifier, BucketPos}, Counts),
            Tags = maps:remove('__name__', Identifier),
            case Cutoff of
                <<"+Inf">> ->
                    lists:reverse([{Tags#{le => Cutoff}, Value+Count} | Acc]);
                _ when is_float(Cutoff) ->
                    get_hist(Identifier, BucketPos+1, Count+Value, [{Tags#{le => imetrics_utils:bin(float_to_list(Cutoff, [short]))}, Value+Count} | Acc], Counts);
                _ ->
                    get_hist(Identifier, BucketPos+1, Count+Value, [{Tags#{le => imetrics_utils:bin(Cutoff)}, Value+Count} | Acc], Counts)
            end;
        [] ->
            []
    end.


calculate_buckets(NumBuckets, Rem, Min, Diff, Acc) when NumBuckets > 1 ->
    calculate_buckets(NumBuckets-1, Rem-Diff, Min, Diff, [Rem | Acc]);
calculate_buckets(_NumBuckets, _Rem, Min, _Diff, Acc) ->
    [Min | Acc].

%% (Re)creating a histogram resets its counts, so clear any shards too.
make_buckets(Identifier, CurrBucket, []) ->
    delete_shards({Identifier, CurrBucket}),
    ets:insert(?TABLE, {{Identifier, CurrBucket}, <<"+Inf">>, 0});
make_buckets(Identifier, CurrBucket, [Cutoff | Tail]) ->
    delete_shards({Identifier, CurrBucket}),
    ets:insert(?TABLE, {{Identifier, CurrBucket}, Cutoff, 0}),
    make_buckets(Identifier, CurrBucket+1, Tail).

-spec shards() -> [pos_integer()].
shards() ->
    lists:seq(1, erlang:system_info(schedulers)).

-spec bucket_count({map(), pos_integer()}) -> non_neg_integer().
bucket_count(BucketKey) ->
    lists:foldl(fun(Shard, Acc) ->
                        case ets:lookup(?COUNTS, {BucketKey, Shard}) of
                            [{_, Count}] -> Acc + Count;
                            [] -> Acc
                        end
                end, 0, shards()).

-spec bucket_total({map(), pos_integer()}, lookup | #{{map(), pos_integer()} => non_neg_integer()}) ->
          non_neg_integer().
bucket_total(BucketKey, lookup) ->
    bucket_count(BucketKey);
bucket_total(BucketKey, Counts) ->
    maps:get(BucketKey, Counts, 0).

%% Sums every bucket's shards in one scan of the counts table.
-spec bucket_totals() -> #{{map(), pos_integer()} => non_neg_integer()}.
bucket_totals() ->
    ets:foldl(fun({{BucketKey, _Shard}, Count}, Acc) ->
                      maps:update_with(BucketKey, fun(Sum) -> Sum + Count end, Count, Acc)
              end, #{}, ?COUNTS).

-spec delete_shards({map(), pos_integer()}) -> ok.
delete_shards(BucketKey) ->
    lists:foreach(fun(Shard) -> ets:delete(?COUNTS, {BucketKey, Shard}) end, shards()).

%% @private Removes one histogram (its bucket definitions, every shard of every
%% bucket, and its name entries), so it no longer appears in any read. Used by
%% imetrics_bench and the tests.
-spec delete(term(), map()) -> ok | {error, term()}.
delete(Name, Tags) ->
    ?CATCH_KNOWN_EXC(
        begin
            TagsWithName = imetrics_utils:bin(Tags#{'__name__' => Name}),
            delete_buckets(TagsWithName, 1),
            case ets:lookup(?TABLE, {names, Name}) of
                [{_, TagMap}] ->
                    case maps:remove(TagsWithName, TagMap) of
                        Empty when map_size(Empty) =:= 0 ->
                            ets:delete(?TABLE, {names, Name}),
                            case ets:lookup(?TABLE, names) of
                                [{names, Names}] -> ets:insert(?TABLE, {names, maps:remove(Name, Names)});
                                [] -> true
                            end;
                        TagMap2 ->
                            ets:insert(?TABLE, {{names, Name}, TagMap2})
                    end;
                [] ->
                    true
            end,
            ok
        end
    ).

-spec delete_buckets(map(), pos_integer()) -> ok.
delete_buckets(Identifier, Pos) ->
    case ets:take(?TABLE, {Identifier, Pos}) of
        [_] ->
            delete_shards({Identifier, Pos}),
            delete_buckets(Identifier, Pos + 1);
        [] ->
            ok
    end.

store_name(Name, Tags) ->
    case ets:lookup(?TABLE, names) of
        [{names, Map}] ->
            ets:insert(?TABLE, {names, Map#{Name => Name}});
        [] ->
            ets:insert(?TABLE, {names, #{Name => Name}})
    end,
    TagsWithName = imetrics_utils:bin(Tags#{ '__name__' => Name}),
    case ets:lookup(?TABLE, {names, Name}) of
        [{_, TagMap}] ->
            ets:insert(?TABLE, {{names, Name}, TagMap#{TagsWithName => TagsWithName}});
        [] ->
            ets:insert(?TABLE, {{names, Name}, #{TagsWithName => TagsWithName}})
    end.

find_bucket(Identifier, Value, Pos) ->
    case ets:lookup(?TABLE, {Identifier, Pos}) of
        [{_, Cutoff, _Value}] ->
            case Cutoff of
                C when C >= Value ->
                    Pos;
                _ ->
                    find_bucket(Identifier, Value, Pos+1)
            end;
        [] ->
            error(badarg)
    end.