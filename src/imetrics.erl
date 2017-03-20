-module(imetrics).

-include("../include/imetrics.hrl").

-export([add/1, add/2, add_m/2, add_m/3]).

-export([set_gauge/2, set_gauge_m/3]).

-export([get/0]).

-compile({no_auto_import,[get/0]}).

-define(CATCH_KNOWN_EXC(X), try
        X
    catch
        error:badarg ->
            % Fail silent if we receive badarg -- highly likely that in a weird
            % application state, ets tables can disappear, so we don't want our
            % metrics collecting to crash the application
            {error, {badarg, check_ets}};
        error:function_clause ->
            {error, {function_clause, check_inputs}};
        error:{badmatch, false} ->
            {error, {badmatch, check_inputs}};
        error:undef ->
            {error, {undef, check_erlang_vsn}}
    end).

%% ---------------------------------------------------------
%% Note: consider using counters over gauges where possible.
%% They are usually easier to intrument in code, and they
%% do not hide local extrema when quantized.
%% ---------------------------------------------------------

add(Name) ->
    add(Name, 1).

add(Name, Value) ->
    ?CATCH_KNOWN_EXC(
        begin
            true = is_integer(Value),
            NameBin = bin(Name),
            ets:update_counter(imetrics_counters, NameBin, Value, {NameBin, 0})
        end
    ).

add_m(Name, Key) ->
    add_m(Name, Key, 1).

add_m(Name, Key, Value) ->
    ?CATCH_KNOWN_EXC(
        begin
            true = is_integer(Value),
            NameBin = bin(Name),
            KeyBin = bin(Key),
            Id = mapped_id(NameBin, KeyBin),
            ets:update_counter(imetrics_mapped_counters, Id, Value, {Id, 0})
        end
    ).

set_gauge(Name, Value) ->
    ?CATCH_KNOWN_EXC(
        begin
            NameBin = bin(Name),
            ets:insert(imetrics_gauges, {NameBin, Value}),
            Value
        end
    ).

set_gauge_m(Name, Key, Value) when is_number(Value); is_function(Value) ->
    ?CATCH_KNOWN_EXC(
        begin
            NameBin = bin(Name),
            KeyBin = bin(Key),
            Id = mapped_id(NameBin, KeyBin),
            ets:insert(imetrics_mapped_gauges, {Id, Value}),
            Value
        end
    ).

get() ->
    Counters = get_unmapped(imetrics_counters),
    Gauges = get_unmapped(imetrics_gauges),
    MappedCounters = get_mapped(imetrics_mapped_counters),
    MappedGauges = get_mapped(imetrics_mapped_gauges),
    Counters ++ Gauges ++ MappedCounters ++ MappedGauges.
    
%% ---
bin(V) when is_atom(V) ->
    bin(atom_to_list(V));
bin(V) when is_list(V) ->
    list_to_binary(V);
bin(V) when is_binary(V) ->
    V;
bin(V) when is_integer(V) ->
    bin(integer_to_list(V));
bin(T) when is_tuple(T) andalso tuple_size(T) =< 8 ->
    L = tuple_to_list(T),
    L2 = [ bin(X) || X <- L ],
    binary_join(L2, application:get_env(imetrics, separator, <<"_">>)).

mapped_id(Name, Key) when is_binary(Name), is_binary(Key) ->
    {Name, Key}.

get_unmapped(T) ->
    Acc = ets:foldl(fun({Name, Value}, Acc0) ->
                Value2 = if is_function(Value) -> Value();
                    true -> Value
                end,
                [{Name, Value2}|Acc0]
        end, [],
        T),
    lists:reverse(Acc).

get_mapped(T) ->
    MappedDict = ets:foldl(fun({{Name, Key}, Value}, MappedDict0) ->
                Value2 = if is_function(Value) -> Value();
                    true -> Value
                end,
                orddict:append_list(Name, [{Key, Value2}], MappedDict0)
        end, orddict:new(),
        T),
    orddict:to_list(MappedDict).

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
    <<>>;
binary_join([Part], _Sep) ->
    Part;
binary_join(List, Sep) ->
    lists:foldr(fun (A, B) ->
                if
                    bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
                    true -> A
                end
        end, <<>>, List).
