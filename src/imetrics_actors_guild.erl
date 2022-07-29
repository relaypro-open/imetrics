-module(imetrics_actors_guild).
-behaviour(gen_server).

-define(DumpTimeout, 100).

-export([start_link/1, set_gauge/4, unset_gauge/3, dump/2, init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

start_link(SvrRef) ->
    gen_server:start_link(SvrRef, ?MODULE, #{}, []).

set_gauge(Svr, Actor, Name, Value) when is_number(Value) ->
    gen_server:cast(Svr, {set_gauge, Actor, Name, Value}).

unset_gauge(Svr, Actor, Name) ->
    gen_server:cast(Svr, {unset_gauge, Actor, Name}).

dump(Svr, AggKey) ->
    try gen_server:call(Svr, {dump, AggKey}, ?DumpTimeout)
    catch 'exit':{timeout, _Call} ->
              []
    end.

init(#{}) ->
    {ok, #{gauges => #{},
           sums => #{},
           counts => #{},
           monitors => #{}}}.

handle_call({dump, AggKey}, _From, State) ->
    case maps:get(AggKey, State, undefined) of
        undefined ->
            {reply, [], State};
        Vals ->
            {reply, maps:to_list(Vals), State}
    end.

handle_cast({set_gauge, Actor, Name, Value}, State) ->
    {ActorRef, State2} = ensure_monitor(Actor, State),
    {noreply, set_actor_gauge(ActorRef, Name, Value, State2)};
handle_cast({unset_gauge, Actor, Name}, State) ->
    {ActorRef, State2} = ensure_monitor(Actor, State),
    {noreply, unset_actor_gauge(ActorRef, Name, State2)}.

handle_info({'DOWN', DownMRef, process, DownPid, _Info}, State=#{monitors := Monitors}) ->
    DownActorRef = actor_ref(DownPid, DownMRef),
    State2 = remove_actor(DownActorRef, State),
    {noreply, State2#{monitors => maps:without([DownPid], Monitors)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extras) ->
    {ok, State}.

actor_ref(Pid, MRef) -> {Pid, MRef}.

ensure_monitor(Actor, State=#{monitors := Monitors}) ->
    case maps:get(Actor, Monitors, undefined) of
        undefined ->
            MRef = erlang:monitor(process, Actor),
            {actor_ref(Actor, MRef), State#{monitors => Monitors#{Actor => MRef}}};
        MRef ->
            {actor_ref(Actor, MRef), State}
    end.

set_actor_gauge(ActorRef, Name, Value, State=#{gauges := Gauges, sums := Sums, counts := Counts}) ->
    Sum = maps:get(Name, Sums, 0),
    Count = maps:get(Name, Counts, 0),

    ActorGauges = maps:get(ActorRef, Gauges, #{}),
    OldVal = maps:get(Name, ActorGauges, 0),

    Sum2 = Sum - OldVal + Value,
    Count2 = Count + 1,

    ActorGauges2 = ActorGauges#{Name => Value},
    Sums2 = Sums#{Name => Sum2},
    Counts2 = Counts#{Name => Count2},
    Gauges2 = Gauges#{ActorRef => ActorGauges2},

    State#{gauges => Gauges2, sums => Sums2, counts => Counts2}.

unset_actor_gauge(ActorRef, Name, State=#{gauges := Gauges, sums := Sums, counts := Counts}) ->
    ActorGauges = maps:get(ActorRef, Gauges, #{}),
    case maps:get(Name, ActorGauges, undefined) of
        undefined ->
            State;
        Value ->
            Sum = maps:get(Name, Sums, 0),
            Count = maps:get(Name, Counts, 0),
            State#{gauges => maps:without([ActorRef], Gauges),
                   sums => Sums#{Name => Sum - Value},
                   counts => Counts#{Name => Count - 1}}
    end.

remove_actor(ActorRef, State=#{gauges := Gauges, sums := Sums, counts := Counts}) ->
    ActorGauges = maps:get(ActorRef, Gauges, #{}),
    {Sums2, Counts2} = lists:foldl(
      fun({Name, Value}, {SumsAcc, CountsAcc}) ->
              Sum = maps:get(Name, SumsAcc, 0),
              Count = maps:get(Name, CountsAcc, 0),
              {SumsAcc#{Name => Sum - Value},
               CountsAcc#{Name => Count - 1}}
      end,
      {Sums, Counts},
      maps:to_list(ActorGauges)),
    State#{sums => Sums2,
           counts => Counts2,
           gauges => maps:without([ActorRef], Gauges)}.
