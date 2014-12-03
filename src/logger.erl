-module(logger).

-behaviour(gen_server).

-include("parallant.hrl").
%% API
-export([start_link/1, start/2, stop/1, log/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {algorithm :: algorithm(),
                env :: environment(),
                step = 1 :: pos_integer(),
                world_impl :: world_impl(),
                log = false :: boolean(),
                animate = true :: boolean()}).

-define(LOG_DELAY, 0). % ms
-define(MAX_WIDTH_TO_SHOW, 65).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Params) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Params, []).

log(Env) ->
    gen_server:cast(?SERVER, {log, Env}).

start(Env, C) ->
    start_link([C#config.algorithm, Env, C#config.world_impl,
                C#config.log, C#config.animate]).

stop(EndEnv) ->
    gen_server:call(?SERVER, {stop, EndEnv}, infinity).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Algorithm, Env, WorldImpl]) ->
    init([Algorithm, Env, WorldImpl, false, true]);
init([Algorithm, Env, WorldImpl, Log]) ->
    init([Algorithm, Env, WorldImpl, Log, true]);
init([Algorithm, Env, WorldImpl, Log, Animate]) ->
    print(Algorithm, Env, WorldImpl, 1),
    {ok, #state{algorithm = Algorithm,
                log = Log,
                world_impl = WorldImpl,
                animate = Animate,
                env = Env}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({stop, Env}, _From, State) ->
    print(State#state.algorithm, Env, State#state.world_impl, State#state.step),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({log, Env}, S = #state{log = false}) ->
    noreply_log(S#state{env = Env});
handle_cast({log, Env}, S) ->
    log(S#state.algorithm, Env, S#state.world_impl,
        S#state.step, S#state.animate),
    noreply_log(S#state{env = Env});
handle_cast(_Msg, S) ->
    {noreply, S}.

noreply_log(S) ->
    {noreply, S#state{step = S#state.step + 1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec overwrite_display(boolean(), pos_integer()) -> ok.
overwrite_display(true, Height) ->
    % display in the same place as the previous step
    io:format("\033[~pA", [Height + 2]);
overwrite_display(false, _) ->
    ok.

-spec log(algorithm(), environment(), world_impl(), pos_integer(), boolean()) ->
                 ok.
log(Algorithm, Env, WorldImpl, Step, Animate) ->
    io:format("Step ~p:~n", [Step + 1]),
    %% algorithm:display(Algorithm, Env, WorldImpl),
    io:format("Agents:~n~p~n",[lists:sort(Env#env.agents)]),
    timer:sleep(?LOG_DELAY),
    overwrite_display(Animate, get_height(Env)).

-spec print(algorithm(), environment(), world_impl(), pos_integer()) -> ok.
print(Algorithm, E = #env{world = W}, WorldImpl, Steps) ->
    #world{w = Width} = W,
    case Width < ?MAX_WIDTH_TO_SHOW of
        true ->
            %% io:format("Ants: ~p~n", [E#env.agents]),
            io:format("Step ~p:~n", [Steps]), %%,
            %% algorithm:display(Algorithm, E, WorldImpl)
            io:format("Agents:~n~p~n",[lists:sort(E#env.agents)]);
        false -> ok
    end.

-spec get_height(environment()) -> pos_integer().
get_height(#env{world = #world{h = Height}}) ->
    Height.
