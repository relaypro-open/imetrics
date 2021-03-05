-module(quickselect_tests).
-include_lib("eunit/include/eunit.hrl").

run_test() ->
    ?assertMatch([0,1,2,3,4,5,6,7,8,9], t([9, 8, 7, 6, 5, 0, 1, 2, 3, 4])),
    ?assertMatch(103, quickselect:select(3, lists:seq(100, 150))),
    ?assertMatch({d,3}, quickselect:select(fun({_,A},{_,B}) -> A =< B end,
                                                3, lists:zip([a,b,c,d,e,f,g,h,i,j], lists:seq(0, 9)))),
    ?assertMatch(3, quickselect:select(3, [0,1,3,3,3,3,6,7,8,9])).

t(V) ->
    lists:map(
      fun(I) -> quickselect:select(I,V) end, 
      lists:seq(0, length(V) - 1)
     ).
