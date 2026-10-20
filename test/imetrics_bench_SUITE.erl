%% On-demand concurrency benchmark; see imetrics_bench for details.
%%
%% all/0 is empty, so a plain `rebar3 ct` (as in CI) runs nothing here.
%% Run the `bench` group explicitly:
%%
%%   rebar3 ct --suite=test/imetrics_bench_SUITE --group=bench
-module(imetrics_bench_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([run/1]).

all() ->
    [].

groups() ->
    [{bench, [], [run]}].

init_per_group(bench, Config) ->
    application:load(imetrics),
    application:set_env(imetrics, require_http_server_on_startup, false),
    {ok, _} = application:ensure_all_started(imetrics),
    Config.

end_per_group(bench, _Config) ->
    application:stop(imetrics),
    application:unset_env(imetrics, require_http_server_on_startup),
    ok.

run(_Config) ->
    Result = imetrics_bench:run(),
    ct:pal("~p", [Result]),
    case Result of
        #{pass := true} -> ok;
        #{pass := false} -> ct:fail({bench_failed, maps:get(cases, Result)})
    end.
