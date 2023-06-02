-module(imetrics_hist_openmetrics).

-export([new/2, new/3, new/4, add/2, add/3, get_hist/1, get_hist/2, get_all/0]).

-export([set_exemplar/2, set_exemplar/3, set_exemplar/4, set_exemplar/5]).

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
            case ets:lookup(?MODULE, {names, Name}) of
                [{_, _}] ->
                    Bucket = find_bucket(TagsWithName, EValue, 1),
                    [{_, BucketVal, _}] = ets:lookup(?MODULE, {TagsWithName, Bucket}),
                    BucketVal2 = case BucketVal of
                        _ when is_float(BucketVal) -> imetrics_utils:bin(float_to_list(BucketVal, [short]));
                        _ -> imetrics_utils:bin(BucketVal) end,
                    TagsWithLabel = imetrics_utils:bin(TagsWithName#{le => BucketVal2}),
                    ets:insert(imetrics_exemplars, {TagsWithLabel, EValue, BinList, Timestamp});
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
            ets:update_counter(?MODULE, {TagsWithName, Bucket}, {3, 1})
        end
    ).

new(Name, Buckets) ->
    new(Name, #{}, Buckets).
new(Name, Tags, Buckets) when is_map(Tags) ->
    ?CATCH_KNOWN_EXC(
        begin
            TagsWithName = imetrics_utils:bin(Tags#{ '__name__' => Name}),
            make_buckets(TagsWithName, 1, Buckets),
            store_name(Name, Tags)
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
            case {NumBuckets, Delta} of
                {0, _} ->
                    make_buckets(TagsWithName, 1, []),
                    store_name(Name, Tags);
                {1, 0} ->
                    make_buckets(TagsWithName, 1, [Min]),
                    store_name(Name, Tags);
                {_, 0} ->
                    error(badarith);
                {_, _} ->
                    Diff = Delta/(NumBuckets - 1),
                    make_buckets(TagsWithName, 1, calculate_buckets(Min, Max, Diff, [])),
                    store_name(Name, Tags)
            end
        end  
    ).

get_all() ->
    case ets:lookup(?MODULE, names) of
        [{names, Names}] ->
            maps:fold(
                fun(_, Name, Acc) -> 
                    Group = case ets:lookup(?MODULE, {names, Name}) of
                        [{_, Identifiers}] -> 
                            maps:fold(
                                fun(_, Identifier, Acc2) -> 
                                    get_hist(Identifier) ++ Acc2
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
    case ets:lookup(?MODULE, {Identifier, BucketPos}) of
        [{_, Cutoff, Value}] ->
            Tags = maps:remove('__name__', Identifier),
            case Cutoff of
                <<"+Inf">> ->
                    lists:reverse([{Tags#{le => Cutoff}, Value+Count} | Acc]);
                _ when is_float(Cutoff) ->
                    get_hist(Identifier, BucketPos+1, Count+Value, [{Tags#{le => imetrics_utils:bin(float_to_list(Cutoff, [short]))}, Value+Count} | Acc]);
                _ ->
                    get_hist(Identifier, BucketPos+1, Count+Value, [{Tags#{le => imetrics_utils:bin(Cutoff)}, Value+Count} | Acc])
            end;
        [] ->
            []
    end.


calculate_buckets(Bucket, Rem, Diff, Acc) when Rem >= 0 ->
    calculate_buckets(Bucket+Diff, Rem-Diff, Diff, [Rem | Acc]);
calculate_buckets(_Bucket, _Rem, _Diff, Acc) ->
    Acc.

make_buckets(Identifier, CurrBucket, []) ->
    ets:insert(?MODULE, {{Identifier, CurrBucket}, <<"+Inf">>, 0});
make_buckets(Identifier, CurrBucket, [Cutoff | Tail]) ->
    ets:insert(?MODULE, {{Identifier, CurrBucket}, Cutoff, 0}),
    make_buckets(Identifier, CurrBucket+1, Tail).

store_name(Name, Tags) ->
    case ets:lookup(?MODULE, names) of
        [{names, Map}] ->
            ets:insert(?MODULE, {names, Map#{Name => Name}});
        [] ->
            ets:insert(?MODULE, {names, #{Name => Name}})
    end,
    TagsWithName = imetrics_utils:bin(Tags#{ '__name__' => Name}),
    case ets:lookup(?MODULE, {names, Name}) of
        [{_, TagMap}] ->
            ets:insert(?MODULE, {{names, Name}, TagMap#{TagsWithName => TagsWithName}});
        [] ->
            ets:insert(?MODULE, {{names, Name}, #{TagsWithName => TagsWithName}})
    end.

find_bucket(Identifier, Value, Pos) ->
    case ets:lookup(?MODULE, {Identifier, Pos}) of
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