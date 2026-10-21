-module(imetrics_shard_tests).
-include_lib("eunit/include/eunit.hrl").

%% Correctness of scheduler-sharded counters and histograms: every read must
%% sum the shards and return exactly what the unsharded implementation did.

-define(PROCS, 32).
-define(CALLS, 10000).

start() ->
    application:load(imetrics),
    application:set_env(imetrics, http_server_port, 8088),
    % other suites may leave these set
    application:unset_env(imetrics, strict_openmetrics_compat),
    application:unset_env(imetrics, openmetrics_exemplar_compat),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(imetrics),
    ok = imetrics_http_server:await(1000),
    #{}.

stop(_) ->
    application:stop(imetrics).

shard_test_() ->
    {foreach, fun start/0, fun stop/1, [
        fun single_process/1,
        fun many_processes/1,
        fun dimensions/1,
        fun fixture_shape/1,
        fun histograms/1,
        fun histogram_reset/1,
        fun delete/1,
        fun restart_clears/1,
        fun shard_rows/1,
        fun exemplars/1,
        fun hist_get_all/1,
        fun table_options/1
    ]}.

%% ---------------------------------------------------------------------------

single_process(_) ->
    [imetrics:add(single) || _ <- lists:seq(1, ?CALLS)],
    ?_assertEqual(?CALLS, proplists:get_value(<<"single">>, imetrics:get_counters())).

many_processes(_) ->
    par(fun() -> imetrics:add(many_one) end),
    par(fun() -> imetrics:add(many_seven, 7) end),
    par(fun() -> imetrics:add(many_tagged, #{t => "x"}, 7) end),
    Ones = ?PROCS * ?CALLS,
    Sevens = 7 * ?PROCS * ?CALLS,
    Counters = imetrics:get_counters(),
    Typed = imetrics:get_with_types(),
    Legacy = imetrics:get(),
    Body = http_get("/metrics"),
    Varz = http_get("/imetrics/varz:counters"),
    [
        ?_assertEqual(Ones, proplists:get_value(<<"many_one">>, Counters)),
        ?_assertEqual(Sevens, proplists:get_value(<<"many_seven">>, Counters)),
        ?_assertEqual({counter, [{#{}, Ones}]}, proplists:get_value(<<"many_one">>, Typed)),
        ?_assertEqual({counter, [{#{}, Sevens}]}, proplists:get_value(<<"many_seven">>, Typed)),
        ?_assertEqual({counter, [{#{t => <<"x">>}, Sevens}]}, proplists:get_value(<<"many_tagged">>, Typed)),
        ?_assertEqual(Ones, proplists:get_value(<<"many_one">>, Legacy)),
        ?_assertEqual(Sevens, proplists:get_value(<<"many_seven">>, Legacy)),
        ?_assert(has_line("many_one{} " ++ integer_to_list(Ones), Body)),
        ?_assert(has_line("many_seven{} " ++ integer_to_list(Sevens), Body)),
        ?_assert(has_line("many_tagged{t=\"x\"} " ++ integer_to_list(Sevens), Body)),
        ?_assert(has_line("many_seven " ++ integer_to_list(Sevens), Varz))
    ].

dimensions(_) ->
    par(fun() -> imetrics:add(dim_tagged, #{code => "200"}) end),
    par(fun() -> imetrics:add(dim_tagged, #{code => "404"}, 7) end),
    par(fun() -> imetrics:add_m(dim_mapped, ok) end),
    par(fun() -> imetrics:add_m(dim_mapped, error, 7) end),
    imetrics:set_counter_dimension(dim_mapped, result),
    N = ?PROCS * ?CALLS,
    [
        ?_assertEqual({counter, [{#{code => <<"200">>}, N}, {#{code => <<"404">>}, 7 * N}]},
                      proplists:get_value(<<"dim_tagged">>, imetrics:get_with_types())),
        ?_assertEqual([{<<"$dim">>, <<"result">>}, {<<"error">>, 7 * N}, {<<"ok">>, N}],
                      proplists:get_value(<<"dim_mapped">>, imetrics:get_counters())),
        ?_assertEqual([{<<"error">>, 7 * N}, {<<"ok">>, N}],
                      proplists:get_value(<<"dim_mapped">>, imetrics:get()))
    ].

%% The scenario was run against the unsharded implementation (commit 50c809a)
%% from one process, and its reads were captured verbatim below. Here every
%% call runs in its own process so the writes land on different shards.
fixture_shape(_) ->
    lists:foreach(fun(F) -> in_proc(F) end, fixture_scenario()),
    Typed = [{Name, {Type, lists:sort(Points)}} || {Name, {Type, Points}} <- imetrics:get_with_types()],
    [
        ?_assertEqual(fixture_get_counters(), imetrics:get_counters()),
        ?_assertEqual(fixture_get(), imetrics:get()),
        % get_with_types/0 points were in ETS hash order before; they are sorted now
        ?_assertEqual(fixture_get_with_types(), Typed)
    ].

histograms(_) ->
    imetrics_hist_openmetrics:new(h, [1, 10, 100]),
    imetrics_hist_openmetrics:new(h, #{t => a}, [1, 10, 100]),
    par(fun() -> imetrics_hist_openmetrics:add(h, 5) end),
    par(fun() -> imetrics_hist_openmetrics:add(h, 1000) end),
    par(fun() -> imetrics_hist_openmetrics:add(h, #{t => a}, 0) end),
    par(fun() -> imetrics:tock(imetrics:tick(h, #{t => a}, second)) end),
    N = ?PROCS * ?CALLS,
    Expected = [{#{le => <<"1">>}, 0},
                {#{le => <<"10">>}, N},
                {#{le => <<"100">>}, N},
                {#{le => <<"+Inf">>}, 2 * N}],
    ExpectedTagged = [{#{le => <<"1">>, t => <<"a">>}, 2 * N},
                      {#{le => <<"10">>, t => <<"a">>}, 2 * N},
                      {#{le => <<"100">>, t => <<"a">>}, 2 * N},
                      {#{le => <<"+Inf">>, t => <<"a">>}, 2 * N}],
    {histogram, All} = proplists:get_value(<<"h">>, imetrics:get_with_types()),
    Body = http_get("/metrics"),
    [
        ?_assertEqual(Expected, imetrics_hist_openmetrics:get_hist(h)),
        ?_assertEqual(ExpectedTagged, imetrics_hist_openmetrics:get_hist(h, #{t => a})),
        ?_assertEqual(lists:sort(Expected ++ ExpectedTagged), lists:sort(All)),
        ?_assertEqual(proplists:get_value(<<"h">>, imetrics:get_hist()), All),
        % the +Inf bucket is the observation count
        ?_assert(has_line("h_bucket{le=\"+Inf\"} " ++ integer_to_list(2 * N), Body)),
        ?_assert(has_line("h_bucket{le=\"10\"} " ++ integer_to_list(N), Body))
    ].

%% new/2,3,4 on an existing histogram resets its counts, as before sharding.
histogram_reset(_) ->
    imetrics_hist_openmetrics:new(hr, [1]),
    par(fun() -> imetrics_hist_openmetrics:add(hr, 0) end),
    Before = imetrics_hist_openmetrics:get_hist(hr),
    imetrics_hist_openmetrics:new(hr, [1]),
    N = ?PROCS * ?CALLS,
    [
        ?_assertEqual([{#{le => <<"1">>}, N}, {#{le => <<"+Inf">>}, N}], Before),
        ?_assertEqual([{#{le => <<"1">>}, 0}, {#{le => <<"+Inf">>}, 0}], imetrics_hist_openmetrics:get_hist(hr)),
        ?_assertEqual(0, shard_rows(imetrics_hist_counts, {imetrics_utils:bin(#{'__name__' => hr}), 1}))
    ].

delete(_) ->
    par(fun() -> imetrics:add(del_c, #{k => v}) end),
    imetrics:add(del_keep, #{k => v}),
    imetrics_hist_openmetrics:new(del_h, [1]),
    imetrics_hist_openmetrics:new(del_h, #{k => v}, [1]),
    par(fun() -> imetrics_hist_openmetrics:add(del_h, 0) end),
    ok = imetrics:delete_counter(del_c, #{k => v}),
    ok = imetrics_hist_openmetrics:delete(del_h, #{}),
    Hist = imetrics_hist_openmetrics:get_all(),
    [
        ?_assertEqual(undefined, proplists:get_value(<<"del_c">>, imetrics:get_with_types())),
        ?_assertEqual(0, shard_rows(imetrics_counters, imetrics_utils:bin(#{name => del_c, k => v}))),
        ?_assertEqual({counter, [{#{k => <<"v">>}, 1}]}, proplists:get_value(<<"del_keep">>, imetrics:get_with_types())),
        ?_assertEqual([], imetrics_hist_openmetrics:get_hist(del_h)),
        ?_assertEqual(0, shard_rows(imetrics_hist_counts, {imetrics_utils:bin(#{'__name__' => del_h}), 1})),
        % the tagged series of the same histogram is untouched
        ?_assertEqual([{#{le => <<"1">>, k => <<"v">>}, 0}, {#{le => <<"+Inf">>, k => <<"v">>}, 0}],
                      proplists:get_value(<<"del_h">>, Hist)),
        ?_assertEqual(ok, imetrics_hist_openmetrics:delete(del_h, #{k => v})),
        ?_assertEqual(undefined, proplists:get_value(<<"del_h">>, imetrics_hist_openmetrics:get_all()))
    ].

%% Restarting imetrics recreates its tables, dropping every shard.
restart_clears(_) ->
    par(fun() -> imetrics:add(restart_c) end),
    ok = application:stop(imetrics),
    {ok, _} = application:ensure_all_started(imetrics),
    ?_assertEqual(undefined, proplists:get_value(<<"restart_c">>, imetrics:get_counters())).

%% One writer pinned to each online scheduler must produce exactly one shard
%% row per scheduler id, so writes collapsing onto one shard fail here.
shard_rows(_) ->
    pinned(fun() -> imetrics:add(rows_c, #{k => v}) end),
    imetrics_hist_openmetrics:new(rows_h, [1]),
    pinned(fun() -> imetrics_hist_openmetrics:add(rows_h, 0) end),
    Shards = lists:seq(1, erlang:system_info(schedulers_online)),
    [
        ?_assertEqual(Shards, shard_ids(imetrics_counters, imetrics_utils:bin(#{name => rows_c, k => v}))),
        ?_assertEqual(Shards, shard_ids(imetrics_hist_counts, {imetrics_utils:bin(#{'__name__' => rows_h}), 1}))
    ].

%% Each pinned writer sets an exemplar labelled with its scheduler id. The
%% newest timestamp goes to a scheduler in the middle of the range, so a read
%% that returned the first or last shard instead of the newest would fail.
%% Every read (get_exemplar/1, get_exemplars/0 and the /metrics exporter) must
%% return that exemplar, whole, for both counters and histograms, and nothing
%% for a key with no exemplar.
exemplars(_) ->
    imetrics_hist_openmetrics:new(ex_h, [10]),
    N = erlang:system_info(schedulers_online),
    Winner = N div 2 + 1,
    pinned(fun() ->
                   S = erlang:system_info(scheduler_id),
                   Ts = case S of Winner -> N + 1; _ -> S end,
                   imetrics:set_exemplar(ex_c, #{}, 1, #{s => integer_to_list(S)}, Ts, counter),
                   imetrics:set_exemplar(ex_h, #{}, 1, #{s => integer_to_list(S)}, Ts, histogram)
           end),
    Expected = {1, #{s => integer_to_binary(Winner)}, N + 1},
    CKey = imetrics_utils:bin(#{'__name__' => ex_c}),
    HKey = imetrics_utils:bin(#{'__name__' => ex_h, le => <<"10">>}),
    NoneKey = imetrics_utils:bin(#{'__name__' => ex_h, le => <<"+Inf">>}),
    All = imetrics:get_exemplars(),
    Body = http_get("/metrics"),
    [
        ?_assertEqual(Expected, imetrics:get_exemplar(CKey)),
        ?_assertEqual(Expected, imetrics:get_exemplar(HKey)),
        ?_assertEqual(Expected, maps:get(CKey, All, undefined)),
        ?_assertEqual(Expected, maps:get(HKey, All, undefined)),
        ?_assert(has_line("ex_h_bucket{le=\"10\"} 0 # {s=\"" ++ integer_to_list(Winner) ++ "\"} 1 "
                          ++ integer_to_list(N + 1), Body)),
        ?_assertEqual(N, ets:select_count(imetrics_exemplars, [{{{CKey, '_'}, '_', '_', '_'}, [], [true]}])),
        ?_assertEqual(N, ets:select_count(imetrics_exemplars, [{{{HKey, '_'}, '_', '_', '_'}, [], [true]}])),
        % the +Inf bucket never got an exemplar
        ?_assertEqual(undefined, imetrics:get_exemplar(NoneKey)),
        ?_assertNot(maps:is_key(NoneKey, All)),
        ?_assert(has_line("ex_h_bucket{le=\"+Inf\"} 0", Body))
    ].

%% get_all/0 sums bucket shards with one scan of the counts table rather than
%% per-bucket lookups: with a writer pinned to every scheduler it must match
%% get_hist/1 exactly, and must ignore shard rows left past +Inf when a
%% histogram is re-created with fewer buckets.
hist_get_all(_) ->
    imetrics_hist_openmetrics:new(ga, [1, 10]),
    imetrics_hist_openmetrics:new(ga_shrunk, [1, 10]),
    pinned(fun() ->
                   imetrics_hist_openmetrics:add(ga, 5),
                   imetrics_hist_openmetrics:add(ga_shrunk, 50)
           end),
    % ga_shrunk's adds were all in bucket 3 (+Inf); re-creating it with one
    % cutoff makes bucket 2 the +Inf bucket and leaves bucket 3's shards behind
    imetrics_hist_openmetrics:new(ga_shrunk, [1]),
    N = 100 * erlang:system_info(schedulers_online),
    All = imetrics_hist_openmetrics:get_all(),
    Expected = [{#{le => <<"1">>}, 0}, {#{le => <<"10">>}, N}, {#{le => <<"+Inf">>}, N}],
    [
        ?_assertEqual(Expected, proplists:get_value(<<"ga">>, All)),
        ?_assertEqual(imetrics_hist_openmetrics:get_hist(ga), proplists:get_value(<<"ga">>, All)),
        ?_assertEqual([{#{le => <<"1">>}, 0}, {#{le => <<"+Inf">>}, 0}], proplists:get_value(<<"ga_shrunk">>, All)),
        ?_assertEqual(erlang:system_info(schedulers_online),
                      shard_rows(imetrics_hist_counts, {imetrics_utils:bin(#{'__name__' => ga_shrunk}), 3}))
    ].

%% Every table written by many processes uses write_concurrency auto (OTP 25+),
%% which grows its row locks with contention instead of a fixed set.
table_options(_) ->
    Tables = [imetrics_counters, imetrics_gauges, imetrics_map_keys, imetrics_stats,
              imetrics_hist_counts, imetrics_vm_metrics, imetrics_exemplars, imetrics_info],
    [?_assertEqual({T, auto, true},
                   {T, ets:info(T, write_concurrency), ets:info(T, decentralized_counters)})
     || T <- Tables].

%% ---------------------------------------------------------------------------

%% Runs F ?CALLS times in each of ?PROCS processes at once.
par(F) ->
    Self = self(),
    Pids = [spawn_link(fun() ->
                           lists:foreach(fun(_) -> F() end, lists:seq(1, ?CALLS)),
                           Self ! {done, self()}
                       end) || _ <- lists:seq(1, ?PROCS)],
    [receive {done, P} -> ok end || P <- Pids],
    ok.

%% Runs F 100 times in one process bound to each online scheduler.
%% spawn_opt's {scheduler, N} option is undocumented but binds the process.
pinned(F) ->
    Self = self(),
    Pids = [spawn_opt(fun() ->
                          lists:foreach(fun(_) -> F() end, lists:seq(1, 100)),
                          Self ! {done, self()}
                      end, [link, {scheduler, N}])
            || N <- lists:seq(1, erlang:system_info(schedulers_online))],
    [receive {done, P} -> ok end || P <- Pids],
    ok.

in_proc(F) ->
    Self = self(),
    Pid = spawn_link(fun() -> F(), Self ! {done, self()} end),
    receive {done, Pid} -> ok end.

%% Number of shard rows stored for one series key (exact match on the key).
shard_rows(Tab, Key) ->
    ets:select_count(Tab, [{{{'$1', '_'}, '_'}, [{'=:=', '$1', {const, Key}}], [true]}]).

%% Sorted shard ids stored for one series key.
shard_ids(Tab, Key) ->
    lists:sort(ets:select(Tab, [{{{'$1', '$2'}, '_'}, [{'=:=', '$1', {const, Key}}], ['$2']}])).

http_get(Path) ->
    {ok, Port} = imetrics_http_server:port(),
    {ok, {{_, 200, _}, _, Body}} = httpc:request("http://localhost:" ++ integer_to_list(Port) ++ Path),
    Body.

has_line(Line, Body) ->
    lists:member(Line, string:split(Body, "\n", all)).

fixture_scenario() ->
    [
        fun() -> imetrics:add(fx_plain) end,
        fun() -> imetrics:add(fx_plain, 6) end,
        fun() -> imetrics:add(fx_tagged, #{code => "200"}) end,
        fun() -> imetrics:add(fx_tagged, #{code => "200"}, 4) end,
        fun() -> imetrics:add(fx_tagged, #{code => "404"}, 2) end,
        fun() -> imetrics:add(fx_tagged, #{code => "500", method => get}, 3) end,
        fun() -> imetrics:add_m(fx_mapped, b_key, 2) end,
        fun() -> imetrics:add_m(fx_mapped, a_key) end,
        fun() -> imetrics:add_m(fx_mapped, 200, 9) end,
        fun() -> imetrics:add_m(fx_dim, x, 5) end,
        fun() -> imetrics:add_m(fx_dim, y) end,
        fun() -> imetrics:set_counter_dimension(fx_dim, status) end,
        fun() -> imetrics:add({fx, tuple}, 3) end,
        fun() -> imetrics:init_counter(fx_zero) end,
        fun() -> imetrics:init_counter(fx_zero_tagged, #{k => v}) end,
        fun() -> imetrics:add(fx_mixed, #{map_key => "m1"}, 2) end,
        fun() -> imetrics:add(fx_mixed, #{other => "o1"}, 1) end
    ].

fixture_get_counters() ->
    [{<<"fx_dim">>,[{<<"$dim">>,<<"status">>},{<<"x">>,5},{<<"y">>,1}]},
     {<<"fx_mapped">>,
      [{<<"$dim">>,<<"map_key">>},{<<"200">>,9},{<<"a_key">>,1},{<<"b_key">>,2}]},
     {<<"fx_mixed">>,[{<<"$dim">>,<<"map_key">>},{<<"m1">>,2}]},
     {<<"fx_plain">>,7},
     {<<"fx_tagged">>,[{<<"$dim">>,<<"map_key">>}]},
     {<<"fx_tuple">>,3},
     {<<"fx_zero">>,0},
     {<<"fx_zero_tagged">>,[{<<"$dim">>,<<"map_key">>}]}].

fixture_get() ->
    [{<<"fx_dim">>,[{<<"x">>,5},{<<"y">>,1}]},
     {<<"fx_mapped">>,[{<<"200">>,9},{<<"a_key">>,1},{<<"b_key">>,2}]},
     {<<"fx_mixed">>,[{<<"m1">>,2}]},
     {<<"fx_plain">>,7},
     {<<"fx_tuple">>,3},
     {<<"fx_zero">>,0}].

%% captured get_with_types/0 output, with each metric's points sorted
fixture_get_with_types() ->
    [{<<"fx_dim">>,{counter,lists:sort([{#{status => <<"y">>},1},{#{status => <<"x">>},5}])}},
     {<<"fx_mapped">>,
      {counter,lists:sort([{#{map_key => <<"b_key">>},2},
                           {#{map_key => <<"200">>},9},
                           {#{map_key => <<"a_key">>},1}])}},
     {<<"fx_mixed">>,
      {counter,lists:sort([{#{other => <<"o1">>},1},{#{map_key => <<"m1">>},2}])}},
     {<<"fx_plain">>,{counter,[{#{},7}]}},
     {<<"fx_tagged">>,
      {counter,lists:sort([{#{code => <<"404">>},2},
                           {#{code => <<"200">>},5},
                           {#{code => <<"500">>,method => <<"get">>},3}])}},
     {<<"fx_tuple">>,{counter,[{#{},3}]}},
     {<<"fx_zero">>,{counter,[{#{},0}]}},
     {<<"fx_zero_tagged">>,{counter,[{#{k => <<"v">>},0}]}}].
