-module(tile_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-include("parallant.hrl").

-export([start_link/1,
         move_all/4]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

start_link(_Args) ->
    gen_server:start_link(?MODULE, [], []).

move_all(Pid, Agents, Env, Config) ->
    gen_server:call(Pid, {move_all, Agents, Env, Config}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Poolboy callbacks

init([]) ->
    {ok, no_state}.


handle_call(die, _From, State) ->
    {stop, {error, died}, dead, State};

handle_call({move_all, {Tile, Agents}, Env, Config}, _From, State) ->
    Moves = handle_move_all(Tile, Agents, Env, Config),
    {reply, Moves, State}.

handle_cast(Event, State) ->
    {stop, {unsupported_event, Event}, State}.


handle_info(Info, State) ->
    {stop, {unsupported_info, Info}, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal logic

handle_move_all(Tile, Agents, Env, Config) ->
    Positions = agents:get_positions(Agents, Tile, Config),
    Shuffled = algorithm:shuffle(Positions),
    algorithm:move_all(Shuffled, Env, Config).
