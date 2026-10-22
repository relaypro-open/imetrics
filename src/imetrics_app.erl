-module(imetrics_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    persistent_term:put({imetrics, separator}, application:get_env(imetrics, separator, <<"_">>)),
    imetrics_sup:start_link().

stop(_State) ->
    ok.
