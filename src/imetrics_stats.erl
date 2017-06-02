%% -------------------------------------------------------------------
%%
%% stats: Statistics Suite for Erlang
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%
%% Changes:
%% - modified module name
%% - modified to use maps instead of state record
%% - modified to fit into the imetrics architecture
%%
%% -------------------------------------------------------------------
-module(imetrics_stats).

-export([new/0,
         update/2, update_all/2,
         count/1,
         min/1, mean/1, max/1,
         variance/1, sdev/1,
         summary/1]).

%% ===================================================================
%% Public API
%% ===================================================================

new() ->
    #{<<"n">> => 0,
      <<"min">> => 'NaN',
      <<"max">> => 'NaN',
      <<"sum">> => 0,
      <<"sum2">> => 0
     }.

update(Value, State=#{ <<"n">> := N, <<"min">> := Min, <<"max">> := Max, 
                       <<"sum">> := Sum, <<"sum2">> := Sum2}) ->
    State#{
      <<"n">>    := N + 1,
      <<"min">>  := nan_min(Value, Min),
      <<"max">>  := nan_max(Value, Max),
      <<"sum">>  := Sum + Value,
      <<"sum2">> := Sum2 + (Value * Value)
     }.

update_all(Values, State) ->
    lists:foldl(fun(Value, S) -> update(Value, S) end,
                        State, Values).

count(#{<<"n">> := N}) ->
    N.

min(#{<<"min">> := Min}) ->
    Min.

mean(#{<<"n">> := 0}) ->
    'NaN';
mean(#{<<"n">> := N, <<"sum">> := Sum}) ->
    Sum / N.

max(#{<<"max">> := Max}) ->
    Max.

variance(#{<<"n">> := N}) when N < 2 ->
    'NaN';
variance(#{<<"n">> := N, <<"sum">> := Sum, <<"sum2">> := Sum2}) ->
    SumSq = Sum * Sum,
    (Sum2 - (SumSq / N)) / (N - 1).


sdev(State) ->
    case variance(State) of
        'NaN' ->
            'NaN';
        Value ->
            math:sqrt(Value)
    end.

summary(State) ->
    {min(State), mean(State), max(State), variance(State), sdev(State)}.

%% ===================================================================
%% Internal functions
%% ===================================================================

nan_min(V1, 'NaN') -> V1;
nan_min('NaN', V1) -> V1;
nan_min(V1, V2)    -> erlang:min(V1, V2).

nan_max(V1, 'NaN') -> V1;
nan_max('NaN', V1) -> V1;
nan_max(V1, V2)    -> erlang:max(V1, V2).
