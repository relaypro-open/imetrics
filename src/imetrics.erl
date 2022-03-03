-module(imetrics).

-include("../include/imetrics.hrl").

-export([add/1, add/2, add_m/2, add_m/3]).

-export([set_gauge/2, set_gauge_m/3, set_multigauge/2, set_multigauge/3, update_gauge/2, update_gauge_m/3]).

-export([set_counter_dimension/2, register_slo/2]).

-export([hist/3, tick/1, tick/2, tock/1, tock/2, tock_as/2, tick_s/3, tick_s/4, tock_s/2, tock_as_s/3, stop_tick_s/2]).

-export([stats/1, set_stats/2]).

-export([get/0, get_with_types/0, get_counters/0, get_gauges/0, get_hist/0, get_hist_percentiles/2, foldl_slo/3, get_slo/2]).

-export([clean_checkpoints/0]).

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
            NameBin = imetrics_utils:bin(Name),
            ets:update_counter(imetrics_counters, NameBin, Value, {NameBin, 0})
        end
    ).

add_m(Name, Key) ->
    add_m(Name, Key, 1).

add_m(Name, Key, Value) ->
    ?CATCH_KNOWN_EXC(
        begin
            true = is_integer(Value),
            NameBin = imetrics_utils:bin(Name),
            KeyBin = imetrics_utils:bin(Key),
            Id = mapped_id(NameBin, KeyBin),
            ets:update_counter(imetrics_mapped_counters, Id, Value, {Id, 0})
        end
    ).

set_gauge(Name, Value) ->
    ?CATCH_KNOWN_EXC(
        begin
            NameBin = imetrics_utils:bin(Name),
            ets:insert(imetrics_gauges, {NameBin, Value}),
            Value
        end
    ).

set_gauge_m(Name, Key, Value) when is_number(Value); is_function(Value) ->
    ?CATCH_KNOWN_EXC(
        begin
            NameBin = imetrics_utils:bin(Name),
            KeyBin = imetrics_utils:bin(Key),
            Id = mapped_id(NameBin, KeyBin),
            ets:insert(imetrics_mapped_gauges, {Id, Value}),
            Value
        end
    ).

update_gauge(Name, Value) ->
    ?CATCH_KNOWN_EXC(
       begin
           NameBin = imetrics_utils:bin(Name),
           ets:update_counter(imetrics_gauges, NameBin, Value, {NameBin, 0})
       end
      ).

update_gauge_m(Name, Key, Value) ->
    ?CATCH_KNOWN_EXC(
       begin
           NameBin = imetrics_utils:bin(Name),
           KeyBin = imetrics_utils:bin(Key),
           Id = mapped_id(NameBin, KeyBin),
           ets:update_counter(imetrics_mapped_gauges, Id, Value, {Id, 0})
       end
      ).

set_counter_dimension(Name, Value) ->
    ?CATCH_KNOWN_EXC(
       begin
           NameBin = imetrics_utils:bin(Name),
           KeyBin = imetrics_utils:bin(<<"$dim">>),
           Id = mapped_id(NameBin, KeyBin),
           Value2 = if is_number(Value) -> Value;
                       true -> imetrics_utils:bin(Value)
                    end,
           ets:insert(imetrics_mapped_counters, {Id, Value2}),
           Value2
       end
      ).

register_slo(UIdName, Opts) ->
    imetrics_sup:register_slo(UIdName, Opts).

set_multigauge(Name, Fun) when is_function(Fun) ->
    set_multigauge(Name, multigauge, Fun).

set_multigauge(Name, Dimension, Fun) when is_function(Fun)
                                          andalso is_atom(Dimension) ->
    ?CATCH_KNOWN_EXC(
       begin
           WrappedFun = fun() ->
                                case call_metrics_fun(Fun) of
                                    List when is_list(List) ->
                                        [{<<"$dim">>, imetrics_utils:bin(Dimension)}|List];
                                    Other ->
                                        Other
                                end
                        end,
           NameBin = imetrics_utils:bin(Name),
           ets:insert(imetrics_mapped_gauges, {NameBin, WrappedFun}),
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

hist(Name, Range=[_Min, _Max], NumBuckets) when is_integer(NumBuckets) ->
    imetrics_hist:new(Name, Range, NumBuckets).

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
                    imetrics_hist:add(Name, Diff)
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
    [Metric || {_, Metric} <- get_with_types()].

get_with_types() ->
    Counters = [{counter, Metric} || Metric <- get_unmapped(imetrics_counters)],
    Gauges = [{gauge, Metric} || Metric <- get_unmapped(imetrics_gauges)],
    Stats = [{stat, Metric} || Metric <- get_unmapped(imetrics_stats)],
    MappedCounters = [{mapped_counter, Metric} || Metric <- get_mapped(imetrics_mapped_counters)],
    MappedGauges = [{mapped_gauge, Metric} || Metric <- get_mapped(imetrics_mapped_gauges)],
    Counters ++ Gauges ++ MappedCounters ++ MappedGauges ++ Stats.

get_counters() ->
    Counters = get_unmapped(imetrics_counters),
    MappedCounters = get_mapped(imetrics_mapped_counters),
    Counters ++ MappedCounters.

get_gauges() ->
    Gauges = get_unmapped(imetrics_gauges),
    MappedGauges = get_mapped(imetrics_mapped_gauges),
    Gauges ++ MappedGauges.

get_hist() ->
    imetrics_hist:get_all().

foldl_slo(UIdName, F, A) ->
    imetrics_slo:foldl_dump(UIdName, F, A).

get_slo(UIdName, UId) ->
    imetrics_slo:dump(UIdName, UId).

get_hist_percentiles(Key, E) ->
    PercentileList = lists:flatten(percentile_list(E)),
    Now = erlang:monotonic_time(millisecond),
    NowHistData = get_hist(),
    case get_checkpoint({hist_percentiles, Key}) of
        {ok, CheckpointData, CheckpointTime} ->
            set_checkpoint({hist_percentiles, Key}, NowHistData, Now),
            IntervalData = lists:filtermap(
                             fun({HistName, HistDataB}) ->
                                     case proplists:get_value(HistName, CheckpointData) of
                                         undefined ->
                                             false;
                                         HistDataA ->
                                             Diff = imetrics_hist:subtract(HistDataB, HistDataA),
                                             case imetrics_hist:approximate_percentiles(Diff,
                                                                PercentileList) of
                                                 [] ->
                                                     false;
                                                 Percentiles ->
                                                    {true, {HistName, [{<<"$dim">>, <<"pctile">>}|Percentiles]}}
                                             end
                                     end
                             end, NowHistData),
            {Now-CheckpointTime, IntervalData};
        {error, not_found} ->
            set_checkpoint({hist_percentiles, Key}, NowHistData, Now),
            {0, []}
    end.

% hard coded to avoid floating point error
percentile_list(E) when E < 1 -> [0.5];
percentile_list(1) -> [0.1, percentile_list(0), 0.9];
percentile_list(2) -> [0.01, percentile_list(1), 0.99];
percentile_list(3) -> [0.001, percentile_list(2), 0.999];
percentile_list(4) -> [0.0001, percentile_list(3), 0.9999];
percentile_list(5) -> [0.00001, percentile_list(4), 0.99999];
percentile_list(6) -> [0.000001, percentile_list(5), 0.999999];
percentile_list(7) -> [0.0000001, percentile_list(6), 0.9999999];
percentile_list(8) -> [0.00000001, percentile_list(7), 0.99999999];
percentile_list(9) -> [0.000000001, percentile_list(8), 0.999999999];
percentile_list(_) -> percentile_list(9).

set_checkpoint(Key, Data, Now) ->
    ?CATCH_KNOWN_EXC(
       begin
            ets:insert(imetrics_data_checkpoint,
                       {Key, Data, Now})
       end).

get_checkpoint(Key) ->
    ?CATCH_KNOWN_EXC(
       begin
            case ets:lookup(imetrics_data_checkpoint, Key) of
                [{_, Data, Time}] ->
                    {ok, Data, Time};
                _ ->
                    {error, not_found}
            end
       end).

clean_checkpoints() ->
    ?CATCH_KNOWN_EXC(
       begin
           MaxAgeHr = application:get_env(imetrics, checkpoint_max_age_hr, 12),
           Expired = erlang:monotonic_time(millisecond) - timer:hours(MaxAgeHr),
           ExpiredKeys = ets:foldl(
             fun({Key, _, Time}, Acc) ->
                     if Time < Expired ->
                            [Key|Acc];
                        true ->
                            Acc
                     end
             end, [], imetrics_data_checkpoint),
           [ ets:delete(imetrics_data_checkpoint, X) || X <- ExpiredKeys ]
       end).
    
%% ---

mapped_id(Name, Key) when is_binary(Name), is_binary(Key) ->
    {Name, Key}.

call_metrics_fun(Fun) ->
    try
        Fun()
    catch _:_ ->
              -1
    end.

get_unmapped(T) ->
    Acc = ets:foldl(fun({Name, Value}, Acc0) ->
                Value2 = if is_function(Value) -> call_metrics_fun(Value);
                    true -> Value
                end,
                [{Name, Value2}|Acc0]
        end, [],
        T),
    lists:reverse(Acc).

get_mapped(T) ->
    MappedDict = ets:foldl(
                   fun({{Name, Key}, Value}, MappedDict0) ->
                           Value2 = if is_function(Value) -> call_metrics_fun(Value);
                                       true -> Value
                                    end,
                           if Key =:= <<"$dim">> ->
                                  %% always ensure the special $dim field is the first in the list
                                  imetrics_utils:orddict_prepend_list(Name, [{Key, Value2}], MappedDict0);
                              true ->
                                  orddict:append_list(Name, [{Key, Value2}], MappedDict0)
                           end;
                      ({Name, Value}, MappedDict0) when is_function(Value) ->
                           % multigauge
                           List = call_metrics_fun(Value),
                           List2 = lists:map(fun({K0, V0}) -> {imetrics_utils:bin(K0), V0} end, List),
                           orddict:append_list(Name, List2, MappedDict0);
                      (_, MappedDict0) ->
                           MappedDict0
                   end, orddict:new(),
                   T),
    orddict:to_list(MappedDict).

tock_s_name_match(Ticks, Name) ->
    I = maps:iterator(Ticks),
    fun MapMatchFun(none) -> none;
        MapMatchFun(Return={_, Tick, _I2}) when element(1, Tick) =:= Name -> %% See tick/2
            Return;
        MapMatchFun({__K, __V, I2}) ->
            MapMatchFun(maps:next(I2))
    end(maps:next(I)).

