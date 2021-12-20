-module(imetrics_http_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, inets_pid/0, await/1, port/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {inets_pid}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

await(Timeout) ->
    H = 20,
    Steps = Timeout div 20,
    await(H, Steps).

await(_, 0) ->
    {error, down};
await(H, Steps) ->
    case inets_pid() of
        Pid when is_pid(Pid) ->
            ok;
        _ ->
            timer:sleep(H),
            await(Steps-1)
    end.

port() ->
    case inets_pid() of
        Pid when is_pid(Pid) ->
            [{port, Port}] = httpd:info(Pid, [port]),
            {ok, Port};
        Error ->
            Error
    end.

inets_pid() ->
    gen_server:call(?SERVER, {inets_pid}, 5000).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    process_flag(trap_exit, true),
    gen_server:cast(self(), {start}),
    {ok, #state{}}.

handle_call({inets_pid}, _From, State=#state{inets_pid=Pid}) ->
    {reply, Pid, State}.

ensure_dir(Dir) ->
    case file:make_dir(Dir) of
        ok -> ok;
        {error, eexist} -> ok;
        Er -> Er
    end.

handle_cast({start}, State) ->
    StartRes = try
        start_server()
    catch Class:Reason ->
        {error, {exception, Class, Reason}}
    end,
    case StartRes of
        {ok, Pid} ->
            {noreply, State#state{inets_pid=Pid}};
        Er ->
            % TODO - logging
            io:format("~p~n", [Er]),
            Self = self(),
            spawn(fun() ->
                        timer:sleep(5000),
                        gen_server:cast(Self, {start})
                end),
            {noreply, State}
    end.

start_server() ->
    Home = application:get_env(imetrics, http_home, "/tmp/imetrics"),
    ServerRoot = application:get_env(imetrics, http_server_root, Home ++ "/server_root"),
    DocumentRoot = application:get_env(imetrics, http_document_root, Home ++ "/document_root"),
    ok = ensure_dir(Home),
    ok = ensure_dir(ServerRoot),
    ok = ensure_dir(DocumentRoot),
    inets:start(httpd,
               [{port, application:get_env(imetrics, http_server_port, 8085)},
                {server_name, atom_to_list(?MODULE)},
                {server_root, ServerRoot},
                {document_root, DocumentRoot},
                {bind_address, "localhost"},
                {error_log_format, pretty},
                {erl_script_alias, {"/imetrics", [varz, openmetrics]}}]).

handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State}.

terminate(_Reason, _State=#state{inets_pid=Pid}) when is_pid(Pid) ->
    inets:stop(httpd, Pid);
terminate(_Reson, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

