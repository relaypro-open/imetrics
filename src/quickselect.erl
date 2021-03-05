-module(quickselect).
-export([test/1, select/2, select/3]).

test(V) ->
    lists:map(
      fun(I) -> select(I,V) end, 
      lists:seq(0, length(V) - 1)
     ).

select(K, L) ->
    select(fun(A, B) -> A =< B end, K, L).

%% @doc Returns the first item from the input list
%% that compares greater than the first k elements
%% in the sorted list, without sorting the list
select(Compare, K, [X | Xs]=List) when K < length(List) ->
    {Ys, Zs} = lists:partition(fun(E) -> Compare(E, X) end, Xs),
    L = length(Ys),
    if 
        K < L -> 
            select(Compare, K, Ys);
        K > L -> 
            select(Compare, K - L - 1, Zs);
        true -> 
            X
    end.
