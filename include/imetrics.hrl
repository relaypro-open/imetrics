-ifdef(unsupported).
"This Erlang version is not supported".
-endif.

-ifndef(IMETRICS_HRL).
-define(IMETRICS_HRL, true).

%% Compile-time control of metric recording.
%% 
%% Each macro below maps to a function in imetrics or imetrics_hist_openmetrics.
%%
%%     ?IMETRICS_ADD(info, http_requests, #{code => Code}),
%%     ?IMETRICS_HIST_ADD(debug, http_latency, Elapsed),
%% 
%% We support levels debug, info, notice, warning, error, critical,
%% alert, and emergency, but they are controlled independently of logger.
%% IMETRICS_LEVEL defines which calls are compiled in, similar to a log level.
%% A call is compiled in when its level is the configured level or
%% a more severe one. The configured level can also be `all` or `none`.
%%
%%     -define(IMETRICS_LEVEL, info).               % before the include, per module
%%     {erl_opts, [{d, 'IMETRICS_LEVEL', info}]}.   % in your app's rebar.config
%%     {overrides, [{add, some_dep, [{erl_opts,     % a dep override in your app's config
%%          [{d, 'IMETRICS_LEVEL', info}]}]}]}.
%%
%% When compiled out, the call is a no-op. The metric is not recorded, arguments are
%% not evaluated. Avoid depending on the return values of the macro, since when
%% disabled, the value will not be useful, with the exception of tick_s, tock_s:
%%
%%     {Ref, Ticks2} = ?IMETRICS_TICK_S(debug, Ticks, query, millisecond)
%%     {_Elapsed, Ticks3} = ?IMETRICS_TOCK_S(debug, Ticks2, Ref)
%%
%% Use the same level for the two halves of a pair (tick and tock, for
%% instance) so they are compiled in or out together.

-ifndef(IMETRICS_LEVEL).
-define(IMETRICS_LEVEL, all).
-endif.

-define(IMETRICS_CALL_RANK(L),
        (case L of
             debug -> 1; info -> 2; notice -> 3; warning -> 4;
             error -> 5; critical -> 6; alert -> 7; emergency -> 8
         end)).
-define(IMETRICS_CONFIGURED_RANK(L),
        (case L of
             all -> 0;
             debug -> 1; info -> 2; notice -> 3; warning -> 4;
             error -> 5; critical -> 6; alert -> 7; emergency -> 8;
             none -> 9
         end)).

%% Call when the call's level is at least as severe as the configured one,
%% Off otherwise.
-define(IMETRICS_AT(Level, Call, Default),
        case ?IMETRICS_CALL_RANK(Level) >= ?IMETRICS_CONFIGURED_RANK(?IMETRICS_LEVEL) of
            true -> Call;
            false -> Default
        end).

%% imetrics:add/1,2,3
-define(IMETRICS_ADD(Level, A1),
    ?IMETRICS_AT(Level, imetrics:add(A1), ok)).
-define(IMETRICS_ADD(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:add(A1, A2), ok)).
-define(IMETRICS_ADD(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics:add(A1, A2, A3), ok)).

%% imetrics:add_m/2,3
-define(IMETRICS_ADD_M(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:add_m(A1, A2), ok)).
-define(IMETRICS_ADD_M(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics:add_m(A1, A2, A3), ok)).

%% imetrics:set_exemplar/2,3,4,5,6
-define(IMETRICS_SET_EXEMPLAR(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:set_exemplar(A1, A2), ok)).
-define(IMETRICS_SET_EXEMPLAR(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics:set_exemplar(A1, A2, A3), ok)).
-define(IMETRICS_SET_EXEMPLAR(Level, A1, A2, A3, A4),
    ?IMETRICS_AT(Level, imetrics:set_exemplar(A1, A2, A3, A4), ok)).
-define(IMETRICS_SET_EXEMPLAR(Level, A1, A2, A3, A4, A5),
    ?IMETRICS_AT(Level, imetrics:set_exemplar(A1, A2, A3, A4, A5), ok)).
-define(IMETRICS_SET_EXEMPLAR(Level, A1, A2, A3, A4, A5, A6),
    ?IMETRICS_AT(Level, imetrics:set_exemplar(A1, A2, A3, A4, A5, A6), ok)).

%% imetrics:set_info/2
-define(IMETRICS_SET_INFO(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:set_info(A1, A2), ok)).

%% imetrics:init_counter/1,2
-define(IMETRICS_INIT_COUNTER(Level, A1),
    ?IMETRICS_AT(Level, imetrics:init_counter(A1), ok)).
-define(IMETRICS_INIT_COUNTER(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:init_counter(A1, A2), ok)).

%% imetrics:set_gauge/2,3
-define(IMETRICS_SET_GAUGE(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:set_gauge(A1, A2), ok)).
-define(IMETRICS_SET_GAUGE(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics:set_gauge(A1, A2, A3), ok)).

%% imetrics:set_gauge_m/3
-define(IMETRICS_SET_GAUGE_M(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics:set_gauge_m(A1, A2, A3), ok)).

%% imetrics:set_multigauge/2,3
-define(IMETRICS_SET_MULTIGAUGE(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:set_multigauge(A1, A2), ok)).
-define(IMETRICS_SET_MULTIGAUGE(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics:set_multigauge(A1, A2, A3), ok)).

%% imetrics:update_gauge/2,3
-define(IMETRICS_UPDATE_GAUGE(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:update_gauge(A1, A2), ok)).
-define(IMETRICS_UPDATE_GAUGE(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics:update_gauge(A1, A2, A3), ok)).

%% imetrics:update_gauge_m/3
-define(IMETRICS_UPDATE_GAUGE_M(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics:update_gauge_m(A1, A2, A3), ok)).

%% imetrics:set_counter_dimension/2
-define(IMETRICS_SET_COUNTER_DIMENSION(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:set_counter_dimension(A1, A2), ok)).

%% imetrics:register_slo/2
-define(IMETRICS_REGISTER_SLO(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:register_slo(A1, A2), ok)).

%% imetrics:hist/2,3,4
-define(IMETRICS_HIST(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:hist(A1, A2), ok)).
-define(IMETRICS_HIST(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics:hist(A1, A2, A3), ok)).
-define(IMETRICS_HIST(Level, A1, A2, A3, A4),
    ?IMETRICS_AT(Level, imetrics:hist(A1, A2, A3, A4), ok)).

%% imetrics:tick/1,2,3
-define(IMETRICS_TICK(Level, A1),
    ?IMETRICS_AT(Level, imetrics:tick(A1), ok)).
-define(IMETRICS_TICK(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:tick(A1, A2), ok)).
-define(IMETRICS_TICK(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics:tick(A1, A2, A3), ok)).

%% imetrics:tock/1,2
-define(IMETRICS_TOCK(Level, A1),
    ?IMETRICS_AT(Level, imetrics:tock(A1), 0)).
-define(IMETRICS_TOCK(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:tock(A1, A2), 0)).

%% imetrics:tock_as/2
-define(IMETRICS_TOCK_AS(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:tock_as(A1, A2), 0)).

%% imetrics:tick_s/3,4
-define(IMETRICS_TICK_S(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics:tick_s(A1, A2, A3), {ok, A1})).
-define(IMETRICS_TICK_S(Level, A1, A2, A3, A4),
    ?IMETRICS_AT(Level, imetrics:tick_s(A1, A2, A3, A4), {A2, A1})).

%% imetrics:tock_s/2
-define(IMETRICS_TOCK_S(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:tock_s(A1, A2), {0, A1})).

%% imetrics:tock_as_s/3
-define(IMETRICS_TOCK_AS_S(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics:tock_as_s(A1, A2, A3), {0, A1})).

%% imetrics:stop_tick_s/2
-define(IMETRICS_STOP_TICK_S(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:stop_tick_s(A1, A2), A1)).

%% imetrics:stats/1
-define(IMETRICS_STATS(Level, A1),
    ?IMETRICS_AT(Level, imetrics:stats(A1), imetrics_stats:new())).

%% imetrics:set_stats/2
-define(IMETRICS_SET_STATS(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics:set_stats(A1, A2), A2)).

%% imetrics_hist_openmetrics:new/2,3,4
-define(IMETRICS_HIST_NEW(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics_hist_openmetrics:new(A1, A2), ok)).
-define(IMETRICS_HIST_NEW(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics_hist_openmetrics:new(A1, A2, A3), ok)).
-define(IMETRICS_HIST_NEW(Level, A1, A2, A3, A4),
    ?IMETRICS_AT(Level, imetrics_hist_openmetrics:new(A1, A2, A3, A4), ok)).

%% imetrics_hist_openmetrics:add/2,3
-define(IMETRICS_HIST_ADD(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics_hist_openmetrics:add(A1, A2), ok)).
-define(IMETRICS_HIST_ADD(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics_hist_openmetrics:add(A1, A2, A3), ok)).

%% imetrics_hist_openmetrics:set_exemplar/2,3,4,5
-define(IMETRICS_HIST_SET_EXEMPLAR(Level, A1, A2),
    ?IMETRICS_AT(Level, imetrics_hist_openmetrics:set_exemplar(A1, A2), ok)).
-define(IMETRICS_HIST_SET_EXEMPLAR(Level, A1, A2, A3),
    ?IMETRICS_AT(Level, imetrics_hist_openmetrics:set_exemplar(A1, A2, A3), ok)).
-define(IMETRICS_HIST_SET_EXEMPLAR(Level, A1, A2, A3, A4),
    ?IMETRICS_AT(Level, imetrics_hist_openmetrics:set_exemplar(A1, A2, A3, A4), ok)).
-define(IMETRICS_HIST_SET_EXEMPLAR(Level, A1, A2, A3, A4, A5),
    ?IMETRICS_AT(Level, imetrics_hist_openmetrics:set_exemplar(A1, A2, A3, A4, A5), ok)).

-endif.
