-module(imetrics_http_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, cowboy_pid/0, await/1, port/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {cowboy_pid, port}).

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
    case cowboy_pid() of
        Pid when is_pid(Pid) ->
            ok;
        _ ->
            timer:sleep(H),
            await(Steps - 1)
    end.

port() ->
    case cowboy_pid() of
        Pid when is_pid(Pid) ->
            Port = gen_server:call(?SERVER, {port}, 5000),
            {ok, Port};
        Error ->
            Error
    end.

cowboy_pid() ->
    gen_server:call(?SERVER, {cowboy_pid}, 5000).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    process_flag(trap_exit, true),
    gen_server:cast(self(), {start}),
    {ok, #state{}}.

handle_call({cowboy_pid}, _From, State = #state{cowboy_pid = Pid}) ->
    {reply, Pid, State};
handle_call({port}, _From, State = #state{port = Port}) ->
    {reply, Port, State}.

handle_cast({start}, State) ->
    Port = application:get_env(imetrics, http_server_port, 8085),
    StartRes =
        try
            start_server(Port)
        catch
            Class:Reason ->
                {error, {exception, Class, Reason}}
        end,
    case StartRes of
        {ok, Pid} ->
            {noreply, State#state{cowboy_pid = Pid, port = Port}};
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

start_server(Port) ->
    Dispatch = cowboy_router:compile([
        % '_' here is a match for all domain names
        {'_', [{"/imetrics/:func", varz, []}]}
    ]),
    cowboy:start_clear(
        ?MODULE,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ).

handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State}.

terminate(_Reason, _State = #state{cowboy_pid = Pid}) when is_pid(Pid) ->
    cowboy:stop_listener(?MODULE);
terminate(_Reson, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
