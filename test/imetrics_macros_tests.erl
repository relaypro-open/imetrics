-module(imetrics_macros_tests).
-include_lib("eunit/include/eunit.hrl").

%% The macros in include/imetrics.hrl are tested by compiling small probe modules
%% at runtime, one per configured IMETRICS_LEVEL, and running them.

start() ->
    application:load(imetrics),
    application:set_env(imetrics, http_server_port, 0),
    {ok, _} = application:ensure_all_started(imetrics),
    #{}.

stop(_Fixture) ->
    application:stop(imetrics).

%% Compiles a module that includes the header with IMETRICS_LEVEL set to Level
%% (the default when Level is `default`) and warnings treated as errors.
compile_probe(Name, Level, Body) ->
    Define = case Level of
                 default -> "";
                 _ -> io_lib:format("-define(IMETRICS_LEVEL, ~p).~n", [Level])
             end,
    Source = ["-module(", atom_to_list(Name), ").\n",
              "-compile(export_all).\n-compile(nowarn_export_all).\n",
              Define,
              "-include_lib(\"imetrics/include/imetrics.hrl\").\n",
              Body],
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"), "imetrics_macro_probes"),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    File = filename:join(Dir, atom_to_list(Name) ++ ".erl"),
    ok = file:write_file(File, Source),
    try compile:file(File, [binary, return, warnings_as_errors]) of
        {ok, Name, Bin, _Warnings} ->
            {module, Name} = code:load_binary(Name, File, Bin),
            ok;
        Other ->
            {compile_failed, Other}
    after
        file:delete(File)
    end.

probe_name(Prefix, Level) ->
    list_to_atom(Prefix ++ "_" ++ atom_to_list(Level)).

counters() ->
    proplists:get_keys(imetrics:get_counters()).

%% ---------------------------------------------------------------------------
%% Which levels are compiled in

levels_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Body = "go(D, I, E) -> ?IMETRICS_ADD(debug, D), ?IMETRICS_ADD(info, I), ?IMETRICS_ADD(error, E).\n",
             %% configured level -> the levels that are recorded
             Expected = [{default, [debug, info, error]},
                         {all, [debug, info, error]},
                         {debug, [debug, info, error]},
                         {info, [info, error]},
                         {notice, [error]},
                         {error, [error]},
                         {critical, []},
                         {emergency, []},
                         {none, []}],
             [begin
                  Mod = probe_name("imetrics_macro_levels", Conf),
                  Names = [{L, list_to_atom("lvl_" ++ atom_to_list(Conf) ++ "_" ++ atom_to_list(L))} || L <- [debug, info, error]],
                  [?_assertEqual(ok, compile_probe(Mod, Conf, Body)),
                   fun() ->
                           Mod:go(proplists:get_value(debug, Names), proplists:get_value(info, Names), proplists:get_value(error, Names)),
                           Recorded = [L || {L, N} <- Names, lists:member(atom_to_binary(N), counters())],
                           ?assertEqual(Recorded0, Recorded)
                   end]
              end || {Conf, Recorded0} <- Expected]
     end}.

%% ---------------------------------------------------------------------------
%% A call that is compiled out does not evaluate its arguments

arguments_not_evaluated_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Body = "touch() -> self() ! evaluated, 1.\n"
                    "go() -> ?IMETRICS_ADD(debug, evaluated_ctr, #{v => touch()}), ok.\n",
             Evaluated = fun(Mod) ->
                                 Mod:go(),
                                 receive evaluated -> true after 0 -> false end
                         end,
             [?_assertEqual(ok, compile_probe(imetrics_macro_eval_all, all, Body)),
              ?_assertEqual(true, Evaluated(imetrics_macro_eval_all)),
              ?_assertEqual(ok, compile_probe(imetrics_macro_eval_info, info, Body)),
              ?_assertEqual(false, Evaluated(imetrics_macro_eval_info)),
              ?_assertEqual(ok, compile_probe(imetrics_macro_eval_none, none, Body)),
              ?_assertEqual(false, Evaluated(imetrics_macro_eval_none))]
     end}.

%% A variable that only a compiled-out call uses is still used, so there is no
%% unused-variable warning (compile_probe turns warnings into errors).
no_warnings_test_() ->
    Body = "go(OnlyForMetrics, AlsoOnlyForMetrics) ->\n"
           "    ?IMETRICS_ADD(debug, ctr, #{a => OnlyForMetrics}),\n"
           "    ?IMETRICS_HIST_ADD(debug, hst, #{b => AlsoOnlyForMetrics}, 1),\n"
           "    ok.\n",
    [?_assertEqual(ok, compile_probe(imetrics_macro_warn_none, none, Body)),
     ?_assertEqual(ok, compile_probe(imetrics_macro_warn_all, all, Body))].

%% Used as statements, which is the usual way, a compiled-out call must not make
%% the compiler warn that a term is built and never used, whatever value it has.
%% (tick_s is left out: its value, the ref, is the point of calling it.)
statement_position_test_() ->
    Body = "go(X) ->\n"
           "    ?IMETRICS_ADD(debug, a, X),\n"
           "    ?IMETRICS_ADD_M(debug, a, X),\n"
           "    ?IMETRICS_SET_GAUGE(debug, g, X),\n"
           "    ?IMETRICS_SET_INFO(debug, i, X),\n"
           "    ?IMETRICS_SET_EXEMPLAR(debug, e, X),\n"
           "    ?IMETRICS_SET_MULTIGAUGE(debug, m, X),\n"
           "    ?IMETRICS_SET_COUNTER_DIMENSION(debug, g, X),\n"
           "    ?IMETRICS_REGISTER_SLO(debug, u, X),\n"
           "    ?IMETRICS_HIST(debug, h, X),\n"
           "    ?IMETRICS_HIST_ADD(debug, h, X),\n"
           "    ?IMETRICS_HIST_NEW(debug, h, X),\n"
           "    ?IMETRICS_HIST_SET_EXEMPLAR(debug, h, X),\n"
           "    ?IMETRICS_TICK(debug, q),\n"
           "    ?IMETRICS_TOCK(debug, X),\n"
           "    ?IMETRICS_TOCK_S(debug, X, q),\n"
           "    ?IMETRICS_STOP_TICK_S(debug, X, q),\n"
           "    ?IMETRICS_STATS(debug, s),\n"
           "    ?IMETRICS_SET_STATS(debug, s, X),\n"
           "    ok.\n",
    [?_assertEqual(ok, compile_probe(imetrics_macro_stmt_none, none, Body)),
     ?_assertEqual(ok, compile_probe(imetrics_macro_stmt_info, info, Body))].

%% A level that is not a logger level is a compile warning, not a silent no-op.
bad_level_test_() ->
    Body = "go() -> ?IMETRICS_ADD(infoo, ctr).\n",
    [?_assertMatch({compile_failed, _}, compile_probe(imetrics_macro_bad_call, all, Body)),
     ?_assertMatch({compile_failed, _}, compile_probe(imetrics_macro_bad_conf, infoo,
                                                      "go() -> ?IMETRICS_ADD(info, ctr).\n"))].

%% ---------------------------------------------------------------------------
%% Every macro, compiled in and compiled out

%% One call per macro and arity. Metric names start with mac_. Every result is
%% returned so the test can look for an {error, _} from imetrics.
all_calls_body() ->
    "slo() -> ?IMETRICS_REGISTER_SLO(info, mac_slo, #{}).\n"
    "calls(Fun, Labels, Tags, Ts) ->\n"
    "    _ = ?IMETRICS_HIST(info, mac_hist2, [1, 10]),\n"
    "    _ = ?IMETRICS_HIST(info, mac_hist3, Tags, [1, 10]),\n"
    "    {Ref, Ticks} = ?IMETRICS_TICK_S(info, #{}, mac_hist2, second),\n"
    "    {Ref2, Ticks2} = ?IMETRICS_TICK_S(info, Ticks, make_ref(), mac_hist2, second),\n"
    "    {Elapsed, Ticks3} = ?IMETRICS_TOCK_S(info, Ticks2, Ref),\n"
    "    {Elapsed2, Ticks4} = ?IMETRICS_TOCK_AS_S(info, Ticks3, Ref2, mac_hist2),\n"
    "    Ticks5 = ?IMETRICS_STOP_TICK_S(info, Ticks4, Ref),\n"
    "    Stats = ?IMETRICS_STATS(info, mac_stats),\n"
    "    [?IMETRICS_ADD(info, mac_add1),\n"
    "     ?IMETRICS_ADD(info, mac_add2, 2),\n"
    "     ?IMETRICS_ADD(info, mac_add3, Tags, 3),\n"
    "     ?IMETRICS_ADD_M(info, mac_addm2, key),\n"
    "     ?IMETRICS_ADD_M(info, mac_addm3, key, 3),\n"
    "     ?IMETRICS_SET_EXEMPLAR(info, mac_ex2, 3),\n"
    "     ?IMETRICS_SET_EXEMPLAR(info, mac_ex3, 3, Labels),\n"
    "     ?IMETRICS_SET_EXEMPLAR(info, mac_ex4, Tags, 3, Labels),\n"
    "     ?IMETRICS_SET_EXEMPLAR(info, mac_ex5, Tags, 3, Labels, Ts),\n"
    "     ?IMETRICS_SET_EXEMPLAR(info, mac_ex6, Tags, 3, Labels, Ts, counter),\n"
    "     ?IMETRICS_SET_INFO(info, mac_info, Tags),\n"
    "     ?IMETRICS_INIT_COUNTER(info, mac_init1),\n"
    "     ?IMETRICS_INIT_COUNTER(info, mac_init2, Tags),\n"
    "     ?IMETRICS_SET_GAUGE(info, mac_gauge2, 1.5),\n"
    "     ?IMETRICS_SET_GAUGE(info, mac_gauge3, Tags, 2),\n"
    "     ?IMETRICS_SET_GAUGE_M(info, mac_gaugem, key, 3),\n"
    "     ?IMETRICS_SET_MULTIGAUGE(info, mac_mg2, Fun),\n"
    "     ?IMETRICS_SET_MULTIGAUGE(info, mac_mg3, dim, Fun),\n"
    "     ?IMETRICS_UPDATE_GAUGE(info, mac_ug2, 1),\n"
    "     ?IMETRICS_UPDATE_GAUGE(info, mac_ug3, Tags, 1),\n"
    "     ?IMETRICS_UPDATE_GAUGE_M(info, mac_ugm, key, 1),\n"
    "     ?IMETRICS_SET_COUNTER_DIMENSION(info, mac_gaugem, dim),\n"
    "     ?IMETRICS_HIST(info, mac_hist2, [1, 10]),\n"
    "     ?IMETRICS_HIST(info, mac_hist3, Tags, [1, 10]),\n"
    "     ?IMETRICS_HIST(info, mac_hist3r, [0, 10], 2),\n"
    "     ?IMETRICS_HIST(info, mac_hist4, Tags, [0, 10], 2),\n"
    "     ?IMETRICS_TOCK(info, ?IMETRICS_TICK(info, mac_hist2)),\n"
    "     ?IMETRICS_TOCK(info, ?IMETRICS_TICK(info, mac_hist2, second)),\n"
    "     ?IMETRICS_TOCK(info, ?IMETRICS_TICK(info, mac_hist3, Tags, second), fun(_, D) -> D end),\n"
    "     ?IMETRICS_TOCK_AS(info, ?IMETRICS_TICK(info, mac_hist2), mac_hist2),\n"
    "     Ref, Ref2, Elapsed, Elapsed2, Ticks5,\n"
    "     ?IMETRICS_SET_STATS(info, mac_stats, Stats),\n"
    "     ?IMETRICS_HIST_NEW(info, mac_hn2, [1, 10]),\n"
    "     ?IMETRICS_HIST_NEW(info, mac_hn3, Tags, [1, 10]),\n"
    "     ?IMETRICS_HIST_NEW(info, mac_hn3r, [0, 10], 2),\n"
    "     ?IMETRICS_HIST_NEW(info, mac_hn4, Tags, [0, 10], 2),\n"
    "     ?IMETRICS_HIST_ADD(info, mac_hn2, 5),\n"
    "     ?IMETRICS_HIST_ADD(info, mac_hn3, Tags, 5),\n"
    "     ?IMETRICS_HIST_SET_EXEMPLAR(info, mac_hn2, 5),\n"
    "     ?IMETRICS_HIST_SET_EXEMPLAR(info, mac_hn2, 5, Labels),\n"
    "     ?IMETRICS_HIST_SET_EXEMPLAR(info, mac_hn3, Tags, 5, Labels),\n"
    "     ?IMETRICS_HIST_SET_EXEMPLAR(info, mac_hn3, Tags, 5, Labels, Ts)].\n".

%% Passed in, not built in the probe: a term bound only for compiled-out calls
%% is a compiler warning (built but never used).
call_all(Mod) ->
    Mod:calls(fun() -> [{a, 1}] end, #{l => v}, #{t => 1}, 1262304000).

mac_metric_names() ->
    [N || {N, _} <- imetrics:get_with_types(), binary:match(N, <<"mac_">>) =/= nomatch].

all_macros_enabled_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             [?_assertEqual(ok, compile_probe(imetrics_macro_all_enabled, all, all_calls_body())),
              fun() ->
                      Results = call_all(imetrics_macro_all_enabled),
                      ?assertEqual([], [R || {error, _} = R <- Results]),
                      %% each family reached imetrics
                      Names = mac_metric_names(),
                      [?assert(lists:member(N, Names)) || N <- [<<"mac_add1">>, <<"mac_add3">>, <<"mac_gauge2">>,
                                                                 <<"mac_hist2">>, <<"mac_hn2">>, <<"mac_mg2">>]]
              end]
     end}.

all_macros_disabled_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             [?_assertEqual(ok, compile_probe(imetrics_macro_all_disabled, none, all_calls_body())),
              fun() ->
                      Results = call_all(imetrics_macro_all_disabled),
                      ?assertEqual([], [R || {error, _} = R <- Results]),
                      ?assertEqual([], mac_metric_names()),
                      ?assertEqual([], [N || N <- counters(), binary:match(N, <<"mac_">>) =/= nomatch])
              end,
              ?_assertEqual({ok, undefined}, imetrics_macro_all_disabled:slo())]
     end}.

%% What a macro hands back when it is compiled out has the same type as what
%% the imetrics function returns (a number where it returns a counter value,
%% true where it returns true, a reference where it returns one, ...), so code
%% that uses the value keeps working. Compares the two builds of one probe.
kind(X) when is_number(X) -> number;
kind(X) when is_boolean(X) -> X;
kind(X) when is_atom(X) -> atom;
kind(X) when is_binary(X) -> binary;
kind(X) when is_reference(X) -> reference;
kind(X) when is_pid(X) -> pid;
kind(X) when is_function(X) -> function;
kind(X) when is_map(X) -> map;
kind(X) when is_list(X) -> list;
kind(X) when is_tuple(X) -> {tuple, [kind(E) || E <- tuple_to_list(X)]}.

disabled_matches_enabled_types_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             [?_assertEqual(ok, compile_probe(imetrics_macro_types_on, all, all_calls_body())),
              ?_assertEqual(ok, compile_probe(imetrics_macro_types_off, none, all_calls_body())),
              fun() ->
                      On = [kind(R) || R <- call_all(imetrics_macro_types_on)],
                      Off = [kind(R) || R <- call_all(imetrics_macro_types_off)],
                      ?assertEqual(length(On), length(Off)),
                      %% name the macro position that differs
                      ?assertEqual([], [{N, A, B} || {N, A, B} <- lists:zip3(lists:seq(1, length(On)), On, Off), A =/= B])
              end,
              %% register_slo starts a server, so only the compiled-out value is checked;
              %% it has no pid (a tuple holding a call would make a statement warn)
              ?_assertEqual({ok, undefined}, imetrics_macro_types_off:slo())]
     end}.

%% ---------------------------------------------------------------------------
%% What a compiled-out call hands back, where the caller threads it through

disabled_returns_test_() ->
    Body = "tick() -> ?IMETRICS_TICK(debug, q).\n"
           "tock(T) -> ?IMETRICS_TOCK(debug, T).\n"
           "tock_as(T) -> ?IMETRICS_TOCK_AS(debug, T, q2).\n"
           "tick_s(Ts) -> ?IMETRICS_TICK_S(debug, Ts, q, second).\n"
           "tick_s(Ts, R) -> ?IMETRICS_TICK_S(debug, Ts, R, q, second).\n"
           "tock_s(Ts, R) -> ?IMETRICS_TOCK_S(debug, Ts, R).\n"
           "tock_as_s(Ts, R) -> ?IMETRICS_TOCK_AS_S(debug, Ts, R, q2).\n"
           "stop_tick_s(Ts, R) -> ?IMETRICS_STOP_TICK_S(debug, Ts, R).\n"
           "stats() -> ?IMETRICS_STATS(debug, s).\n"
           "set_stats(S) -> ?IMETRICS_SET_STATS(debug, s, S).\n",
    Ticks = #{some => tick},
    [?_assertEqual(ok, compile_probe(imetrics_macro_returns, info, Body)),
     ?_assertEqual({undefined, native, 0}, imetrics_macro_returns:tick()),
     ?_assertEqual(0, imetrics_macro_returns:tock(imetrics_macro_returns:tick())),
     ?_assertEqual(0, imetrics_macro_returns:tock_as(imetrics_macro_returns:tick())),
     ?_assertMatch({Ref, Ticks} when is_reference(Ref), imetrics_macro_returns:tick_s(Ticks)),
     ?_assertEqual({some_ref, Ticks}, imetrics_macro_returns:tick_s(Ticks, some_ref)),
     ?_assertEqual({0, Ticks}, imetrics_macro_returns:tock_s(Ticks, some_ref)),
     ?_assertEqual({0, Ticks}, imetrics_macro_returns:tock_as_s(Ticks, some_ref)),
     ?_assertEqual(Ticks, imetrics_macro_returns:stop_tick_s(Ticks, some_ref)),
     ?_assertEqual(imetrics_stats:new(), imetrics_macro_returns:stats()),
     ?_assertEqual(imetrics_stats:update(5, imetrics_stats:new()),
                   imetrics_macro_returns:set_stats(imetrics_stats:update(5, imetrics_stats:new())))].
