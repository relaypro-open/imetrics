-module(imetrics_hist).

-export([new/3, add/2, dump/1, get/1]).

-export([compute_bucket/3]).

-define(CATCH_KNOWN_EXC(X),
    try
        X
    catch
        error:badarg ->
            {error, {badarg, check_ets}};
        error:badarith ->
            {error, {badarith, check_inputs}}
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
