-module(tile_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-include("parallant.hrl").

-export([start_link/1,
         get_moves/4]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


start_link(_Args) ->
    gen_server:start_link(?MODULE, [], []).


get_moves(Pid, Agents, Env, Config) ->
    gen_server:call(Pid, {get_moves, Agents, Env, Config}).

init([]) ->
    {ok, no_state}.


handle_call(die, _From, State) ->
    {stop, {error, died}, dead, State};

handle_call({get_moves, Agents, Env, Config}, _From, State) ->
    Moves = parallant:get_moves(Env#env{agents = Agents}, Config),
    {reply, Moves, State}.


handle_cast(Event, State) ->
    {stop, {unsupported_event, Event}, State}.


handle_info(Info, State) ->
    {stop, {unsupported_info, Info}, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
