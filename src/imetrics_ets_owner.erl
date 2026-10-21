-module(imetrics_ets_owner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    % imetrics' use case typically involves many more writes than reads. only one or two metrics collectors
    % should be reading from imetrics, even in a large application... while many processes may be writing.
    % 
    % these tables are configured with `write_concurrency` to ensure that individual processes don't lock the
    % whole table when they try to increase a metric. additionally, `decentralized_counters` helps distribute
    % the ETS table accounting (total # of objects, table size) across multiple different schedulers so
    % there's no single shared lock when inserting or deleting new metrics either. `auto` (OTP 25+) grows
    % the number of row locks with contention, where `true` hashes every key onto a fixed set of locks.
    ets:new(imetrics_counters, [public, named_table, {write_concurrency, auto}, {decentralized_counters, true}]),
    ets:new(imetrics_gauges, [public, named_table, {write_concurrency, auto}, {decentralized_counters, true}]),
    ets:new(imetrics_map_keys, [public, named_table, {write_concurrency, auto}, {decentralized_counters, true}]),
    ets:new(imetrics_stats, [public, named_table, {write_concurrency, auto}, {decentralized_counters, true}]),
    % histogram bucket definitions are read by every observe (find_bucket) but only written when a
    % histogram is created, so that table is read-optimized. observations are counted in per-scheduler
    % shard rows in imetrics_hist_counts (see imetrics_hist_openmetrics).
    ets:new(imetrics_hist_openmetrics, [public, named_table, {read_concurrency, true}]),
    ets:new(imetrics_hist_counts, [public, named_table, {write_concurrency, auto}, {decentralized_counters, true}]),
    ets:new(imetrics_vm_metrics, [public, named_table, {write_concurrency, auto}, {decentralized_counters, true}]),
    ets:new(imetrics_exemplars, [public, named_table, {write_concurrency, auto}, {decentralized_counters, true}]),
    ets:new(imetrics_info, [public, named_table, {write_concurrency, auto}, {decentralized_counters, true}]),

    {ok, #{  }}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

