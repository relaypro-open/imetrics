-module(imetrics_utils).

-export([bin/1, orddict_prepend_list/3]).

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

orddict_prepend_list(Key, ValList, Orddict1) ->
    case orddict:find(Key, Orddict1) of
        {ok, Value} when is_list(Value) ->
            orddict:store(Key, ValList ++ Value, Orddict1);
        error ->
            orddict:append_list(Key, ValList, Orddict1)
    end.
