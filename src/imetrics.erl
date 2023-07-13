-module(imetrics).

-include("../include/imetrics.hrl").

-export([add/1, add/2, add/3, set_exemplar/2, set_exemplar/3, set_exemplar/4, set_exemplar/5, set_exemplar/6, add_m/2, add_m/3, set_info/2]).

-export([set_gauge/2, set_gauge/3, set_gauge_m/3, set_multigauge/2, set_multigauge/3, update_gauge/2, update_gauge/3, update_gauge_m/3]).

-export([set_counter_dimension/2, register_slo/2]).

-export([hist/2, hist/3, hist/4, tick/1, tick/2, tock/1, tock/2, tock_as/2, tick_s/3, tick_s/4, tock_s/2, tock_as_s/3, stop_tick_s/2]).

-export([stats/1, set_stats/2]).

-export([get/0, get_with_types/0, get_counters/0, get_gauges/0, get_hist/0, foldl_slo/3, get_slo/2, get_exemplar/1]).

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
    add(Name, #{}, 1).

add(Name, Tags) when is_map(Tags) ->
    add(Name, Tags, 1);
add(Name, Value) ->
    add(Name, #{}, Value).

add(Name, Tags, Value) ->
    ?CATCH_KNOWN_EXC(
        begin
            true = is_integer(Value),
            TagsWithName = imetrics_utils:bin(Tags#{ name => Name }),
            ets:update_counter(imetrics_counters, TagsWithName, Value, {TagsWithName, 0})
        end
    ).

set_exemplar(Name, EValue) ->
    set_exemplar(Name, #{}, EValue, #{}, erlang:system_time(millisecond)/1000, counter).

set_exemplar(Name, Tags, EValue) when is_map(Tags) ->
    set_exemplar(Name, Tags, EValue, #{}, erlang:system_time(millisecond)/1000, counter);
set_exemplar(Name, EValue, Labels) when is_map(Labels) ->
    set_exemplar(Name, #{}, EValue, Labels, erlang:system_time(millisecond)/1000, counter);
set_exemplar(Name, EValue, Timestamp) when is_number(Timestamp) ->
    set_exemplar(Name, #{}, EValue, #{}, Timestamp, counter);
set_exemplar(Name, EValue, Type) ->
    set_exemplar(Name, #{}, EValue, #{}, erlang:system_time(millisecond)/1000, Type).

set_exemplar(Name, Tags, EValue, Labels) when is_map(Labels) ->
    set_exemplar(Name, Tags, EValue, Labels, erlang:system_time(millisecond)/1000, counter);
set_exemplar(Name, Tags, EValue, Timestamp) when is_map(Tags), is_number(Timestamp) ->
    set_exemplar(Name, Tags, EValue, #{}, Timestamp, counter);
set_exemplar(Name, Tags, EValue, Type) when is_map(Tags) ->
    set_exemplar(Name, Tags, EValue, #{}, erlang:system_time(millisecond)/1000, Type);
set_exemplar(Name, EValue, Labels, Timestamp) when is_number(Timestamp) ->
    set_exemplar(Name, #{}, EValue, Labels, Timestamp, counter);
set_exemplar(Name, EValue, Labels, Type) when is_map(Labels) ->
    set_exemplar(Name, #{}, EValue, Labels, erlang:system_time(millisecond)/1000, Type);
set_exemplar(Name, EValue, Timestamp, Type) ->
    set_exemplar(Name, #{}, EValue, #{}, Timestamp, Type).

set_exemplar(Name, Tags, EValue, Labels, Timestamp) when is_number(Timestamp) ->
    set_exemplar(Name, Tags, EValue, Labels, Timestamp, counter);
set_exemplar(Name, Tags, EValue, Labels, Type) when is_map(Labels) ->
    set_exemplar(Name, Tags, EValue, Labels, erlang:system_time(millisecond)/1000, Type);
set_exemplar(Name, Tags, EValue, Timestamp, Type) when is_number(EValue) ->
    set_exemplar(Name, Tags, EValue, #{}, Timestamp, Type);
set_exemplar(Name, EValue, Labels, Timestamp, Type) ->
    set_exemplar(Name, #{}, EValue, Labels, Timestamp, Type).
    
set_exemplar(Name, Tags, EValue, Labels, Timestamp, Type) ->
    ?CATCH_KNOWN_EXC(
        begin
            case Type of
                histogram ->
                    imetrics_hist_openmetrics:set_exemplar(Name, Tags, EValue, Labels, Timestamp);
                counter ->
                    BinList = imetrics_utils:bin(Labels),
                    TagsWithName = imetrics_utils:bin(Tags#{ '__name__' => Name }),
                    ets:insert(imetrics_exemplars, {TagsWithName, EValue, BinList, Timestamp})
            end
        end
    ).

add_m(Name, Key) ->
    add_m(Name, Key, 1).

add_m(Name, Key, Value) ->
    add(Name, #{ map_key => Key }, Value).

set_gauge(Name, Value) ->
    set_gauge(Name, #{}, Value).

set_gauge(Name, Tags, Value) ->
    ?CATCH_KNOWN_EXC(
        begin
            TagsWithName = imetrics_utils:bin(Tags#{ name => Name }),
            ets:insert(imetrics_gauges, {TagsWithName, Value}),
            Value
        end
    ).

set_gauge_m(Name, Key, Value) when is_number(Value); is_function(Value) ->
    set_gauge(Name, #{ map_key => Key }, Value).

% despite its name, this function actually _increments_ a gauge
update_gauge(Name, Value) ->
    update_gauge(Name, #{}, Value).

update_gauge(Name, Tags, Value) ->
    ?CATCH_KNOWN_EXC(
       begin
            TagsWithName = imetrics_utils:bin(Tags#{ name => Name }),
            ets:update_counter(imetrics_gauges, TagsWithName, Value, {TagsWithName, 0})
       end
      ).

update_gauge_m(Name, Key, Value) ->
    update_gauge(Name, #{ map_key => Key }, Value).

set_counter_dimension(Name, Value) ->
    ?CATCH_KNOWN_EXC(
       begin
           NameBin = imetrics_utils:bin(Name),
           Value2 = if is_number(Value) -> Value;
                       true -> imetrics_utils:bin(Value)
                    end,
           ets:insert(imetrics_map_keys, {NameBin, Value2}),
           Value2
       end
      ).

set_info(Name, Tags) ->
    ets:insert(imetrics_info, {imetrics_utils:bin(Name), [{imetrics_utils:bin(Tags), 1}]}).

register_slo(UIdName, Opts) ->
    imetrics_sup:register_slo(UIdName, Opts).

set_multigauge(Name, Fun) when is_function(Fun) ->
    set_multigauge(Name, multigauge, Fun).

set_multigauge(Name, Dimension, Fun) when is_function(Fun)
                                          andalso is_atom(Dimension) ->
    ?CATCH_KNOWN_EXC(
       begin
           NameBin = imetrics_utils:bin(Name),
           TagsWithName = imetrics_utils:bin(#{ name => NameBin }),
           ets:insert(imetrics_map_keys, {NameBin, imetrics_utils:bin(Dimension)}),
           ets:insert(imetrics_gauges, {TagsWithName#{ '_multigauge' => true }, Fun}),
           Fun
       end
      ).

stats(Name) ->
    ?CATCH_KNOWN_EXC(
       begin
           NameBin = imetrics_utils:bin(Name),
           case ets:lookup(imetrics_stats, NameBin) of
               [] ->
                   Stats = imetrics_stats:new(),
                   ets:insert(imetrics_stats, {NameBin, Stats}),
                   Stats;
               [Stats] ->
                   Stats
           end
       end
    ).

set_stats(Name, Stats) ->
    ?CATCH_KNOWN_EXC(
       begin
           NameBin = imetrics_utils:bin(Name),
           ets:insert(imetrics_stats, {NameBin, Stats}),
           Stats
       end
    ).

hist(Name, Buckets) ->
   imetrics_hist_openmetrics:new(Name, Buckets).
hist(Name, Tags, Buckets) when is_map(Tags) ->
   imetrics_hist_openmetrics:new(Name, Tags, Buckets);
hist(Name, Range=[_Min, _Max], NumBuckets) when is_integer(NumBuckets) ->
    imetrics_hist_openmetrics:new(Name, Range, NumBuckets).
hist(Name, Tags, Range=[_Min, _Max], NumBuckets) when is_integer(NumBuckets) ->
   imetrics_hist_openmetrics:new(Name, Tags, Range, NumBuckets).

tick(Name) ->
    tick(Name, microsecond).

tick(Name, Unit) ->
    {Name, Unit, erlang:monotonic_time(Unit)}.

tock(Tick) ->
    tock_as(Tick, '_').

tock(Tick, Fun) ->
    tock_as(Tick, '_', Fun).

tock_as(Tick, NewName) ->
    tock_as(Tick, NewName,
            fun(Name, Diff) ->
                    imetrics_hist_openmetrics:add(Name, Diff)
            end).

tock_as({Name, Unit, Ts}, '_', Fun) when is_function(Fun) ->
    Ts2 = erlang:monotonic_time(Unit),
    Fun(Name, Ts2-Ts);
tock_as({_OldName, Unit, Ts}, NewName, Fun) when is_function(Fun) ->
    Ts2 = erlang:monotonic_time(Unit),
    Fun(NewName, Ts2-Ts);
tock_as(_, _, _) ->
    {error, badarg}.

%% @doc Tracks tick/tock entries with an opaque map.
%%
%% The caller can choose to either use the returned ref for identification,
%% or the Name parameter. Using the Name parameter can cause collisions if
%% tracking multiple metrics of the same name using the same map state.
%%
tick_s(Ticks, Name, Unit) ->
    Ref = make_ref(),
    tick_s(Ticks, Ref, Name, Unit).

tick_s(Ticks, Ref, Name, Unit) ->
    {Ref, Ticks#{Ref => tick(Name, Unit)}}.

%% @doc Stores the tock value for the given ref or Name (See tick_s)
tock_s(Ticks, RefOrName) ->
    tock_as_s(Ticks, RefOrName, '_').

tock_as_s(Ticks, RefOrName, NewName) ->
    case maps:get(RefOrName, Ticks, undefined) of
        undefined ->
            case tock_s_name_match(Ticks, RefOrName) of
                none ->
                    {{error, badarg}, Ticks};
                {Ref, Tick, _} ->
                    {tock_as(Tick, NewName), maps:remove(Ref, Ticks)}
            end;
        Tick ->
            {tock_as(Tick, NewName), maps:remove(RefOrName, Ticks)}
    end.

stop_tick_s(Ticks, RefOrName) ->
    case maps:get(RefOrName, Ticks, undefined) of
        undefined ->
            case tock_s_name_match(Ticks, RefOrName) of
                none ->
                    Ticks;
                {Ref, _Tick, _} ->
                    maps:remove(Ref, Ticks)
            end;
        _Tick ->
            maps:remove(RefOrName, Ticks)
    end.

get() ->
    Metrics = [{MetricName, MetricPoints} || {MetricName, {_Type, MetricPoints}} <- get_with_types()],
    Metrics2 = simplify_unmapped(Metrics),
    Metrics3 = simplify_mapped(Metrics2),
    Metrics3.

get_with_types() ->
    Info = [{MetricName, {info, MetricPoints}} || {MetricName, MetricPoints} <- get_unmapped(imetrics_info)],
    Counters = [{MetricName, {counter, MetricPoints}} || {MetricName, MetricPoints} <- get_mapped(imetrics_counters)],
    Gauges = [{MetricName, {gauge, MetricPoints}} || {MetricName, MetricPoints} <- get_mapped(imetrics_gauges)],
    Stats = [{MetricName, {stat, MetricPoints}} || {MetricName, MetricPoints} <- get_unmapped(imetrics_stats)],
    Histograms = [{MetricName, {histogram, MetricPoints}} || {MetricName, MetricPoints} <- imetrics_hist_openmetrics:get_all()],
    Info ++ Counters ++ Gauges ++ Stats ++ Histograms.

get_counters() ->
    Counters = get_mapped(imetrics_counters),
    Counters2 = simplify_unmapped(Counters),
    Counters3 = simplify_mapped(Counters2, true),
    Counters3.

get_gauges() ->
    Gauges = get_mapped(imetrics_gauges),
    Gauges2 = simplify_unmapped(Gauges),
    Gauges3 = simplify_mapped(Gauges2, true),
    Gauges3.

get_exemplar(Key) ->
    case ets:lookup(imetrics_exemplars, Key) of
        [] ->
            undefined;
        [{_Name, EValue, Labels, Timestamp}] ->
            {EValue, Labels, Timestamp}
    end.

get_hist() ->
    imetrics_hist_openmetrics:get_all().

foldl_slo(UIdName, F, A) ->
    imetrics_slo:foldl_dump(UIdName, F, A).

get_slo(UIdName, UId) ->
    imetrics_slo:dump(UIdName, UId).
    
%% ---

call_metrics_fun(Fun, Default) ->
    try
        Fun()
    catch _:_ ->
              Default
    end.

get_unmapped(T) ->
    Acc = ets:foldl(fun({Name, Value}, Acc0) ->
                Value2 = if is_function(Value) -> call_metrics_fun(Value, -1);
                    true -> Value
                end,
                [{Name, Value2}|Acc0]
        end, [],
        T),
    lists:reverse(Acc).

get_mapped(T) ->
    MappedDict = ets:foldl(
                   fun({#{ name := Name } = Tags, Value}, MappedDict0) when not is_map_key('_multigauge', Tags) ->
                           Value2 = if is_function(Value) -> call_metrics_fun(Value, -1);
                                       true -> Value
                                    end,

                           % if a dimension has been set on a mapped counter/gauge, rename 'map_key' to what that dimension is
                           % this is for backwards compatibility, the suggested path is to set your tags yourself and not use a mapped counter
                           Tags3 = case {maps:find(map_key, Tags), ets:lookup(imetrics_map_keys, Name)} of
                                {{ok, _}, [{Name, Dimension}]} ->
                                    {MapKeyValue, Tags2} = maps:take(map_key, Tags),
                                    Tags2#{ list_to_atom(binary_to_list(Dimension)) => MapKeyValue };
                                _ ->
                                    Tags
                                end,
                           Tags4 = maps:remove(name, Tags3),
                           orddict:append_list(Name, [{Tags4, Value2}], MappedDict0);
                      ({#{ name := Name, '_multigauge' := true }, Value}, MappedDict0) when is_function(Value) ->
                           % multigauge support
                           [{Name, Dimension}] = ets:lookup(imetrics_map_keys, Name),
                           List = call_metrics_fun(Value, []),
                           List2 = lists:map(fun({K0, V0}) -> {#{ list_to_atom(binary_to_list(Dimension)) => imetrics_utils:bin(K0)}, V0} end, List),
                           orddict:append_list(Name, List2, MappedDict0);
                      (_, MappedDict0) ->
                           MappedDict0
                   end, orddict:new(),
                   T),
    orddict:to_list(MappedDict).

% takes metrics that have a single entry with an empty map and simplify them to K/V pairs
simplify_unmapped(List) ->
    lists:reverse(lists:foldl(fun ({Name, MetricPoints}, Acc) -> 
            case MetricPoints of
                [{Tags, Value}] when map_size(Tags) == 0 ->
                    [{Name, Value}|Acc];
                _ ->
                    [{Name, MetricPoints}|Acc]
            end
        end, [], List)).

% simplifies mapped metrics to only include a single key, for backwards compatibility (like imetrics:get/0)
simplify_mapped(List) ->
    simplify_mapped(List, false).
simplify_mapped(List, IncludeDim) ->
    lists:reverse(lists:foldl(fun ({Name, MetricPoints}, Acc) -> 
            if is_list(MetricPoints) ->
                    MapKeys = ets:lookup(imetrics_map_keys, Name),
                    MapKeyBin = proplists:get_value(Name, MapKeys, <<"map_key">>),
                    MapKeyAtom = list_to_atom(binary_to_list(MapKeyBin)),
                    FoldAccStart2 = case IncludeDim of
                        true -> [{<<"$dim">>, MapKeyBin}];
                        false -> []
                    end,
                    MetricPoints2 = lists:sort(lists:foldl(fun ({Tags, Value}, Acc2) ->
                            case maps:get(MapKeyAtom, Tags, undefined) of
                                undefined -> Acc2;
                                MapValue -> [{MapValue, Value}|Acc2]
                            end
                        end, FoldAccStart2, MetricPoints)),
                    case MetricPoints2 of
                        [] -> Acc;
                        _ -> [{Name, MetricPoints2}|Acc]
                    end;
                true ->
                    [{Name, MetricPoints}|Acc]
            end
        end, [], List)).

tock_s_name_match(Ticks, Name) ->
    I = maps:iterator(Ticks),
    fun MapMatchFun(none) -> none;
        MapMatchFun(Return={_, Tick, _I2}) when element(1, Tick) =:= Name -> %% See tick/2
            Return;
        MapMatchFun({__K, __V, I2}) ->
            MapMatchFun(maps:next(I2))
    end(maps:next(I)).

