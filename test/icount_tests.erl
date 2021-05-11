-module(icount_tests).
-include_lib("eunit/include/eunit.hrl").

start() ->
    {ok, C} = icount:start_link(#{size => 32, opts => [], hwm => 32*1024*1024}),
    C.

stop(C) ->
    unlink(C),
    exit(C, brutal_kill).

icount_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun icount_test/1}. 

icount_test(C) ->
    [
     ?_assertMatch(ok, local_add(C)),
     ?_assertMatch(ok, local_cleanup(C))
    ].

local_add(C) ->
    % ensure we have the idx prepped
    ok = icount:add(C, "eunit", test),

    % localize the counters object
    ok = icount:localize(C, "eunit"),

    % induce sleep on gen_server
    C ! {sleep, 100},

    % increment key on localized counters object (idx already known)
    Tick = erlang:monotonic_time(millisecond),
    ok = icount:add(C, "eunit", test),
    Tock = erlang:monotonic_time(millisecond),

    % make sure execution time is less than the sleep time
    if (Tock - Tick) < 5 -> ok end,

    % allow gen_server to exit sleep
    timer:sleep(100),

    % clean up
    ok = icount:delocalize(C, "eunit").

local_cleanup(C) ->
    ok = icount:localize(C, "eunit"),
    [{"eunit", _}] = icount:dump(C),
    #{localized := {#{"eunit" := MRef}, _}} = sys:get_state(C),
    C ! {'DOWN', MRef, process, self(), []}, % fake it
    [] = icount:dump(C),
    ok.
