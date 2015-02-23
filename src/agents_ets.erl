%%%-------------------------------------------------------------------
%%% @author Daniel Grzonka
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% Agents ETS based approach
%%% @end
%%% Created : 28. lis 2014 13:31
%%%-------------------------------------------------------------------
-module(agents_ets).
-author("Daniel").
-behaviour(agents).
%% API
-export([create_agents/3,
         update_agent/4,
         get_agent/3,
         get_positions/2,
         get_list/1,
         get_tiles/2]).

-include("parallant.hrl").

-define(LOAD(Attribute, Proplist, Default),
        Attribute = proplists:get_value(Attribute, Proplist, Default)).

-type agent_state() :: parallant:agent_state().
-type agents() :: agents:agents(ets:tid()).
-type tile() :: agents:tile({Start :: position(), End :: position()}).

-spec create_agents(PopulationSize :: pos_integer(),
                    World :: world(),
                    config()) ->
                           TableID :: agents().
create_agents(PopulationSize, World, Config) ->
    TableName = agentsETS,
    clean(TableName),
    TID = ets:new(TableName, [ordered_set, public, {keypos, #agent.pos}]),
    Pop = model:initial_population(PopulationSize, World, Config),
    Agents = [#agent{pos = Pos, state = State} || {Pos, State} <- Pop],
    ets:insert(TID, Agents),
    TID.

-spec get_agent(position(), environment(), config()) ->
                       agent_state().
get_agent(Pos, Env, _Config) ->
    case ets:lookup(Env#env.agents, Pos) of
        [] ->
            empty;
        [A] ->
            A#agent.state
    end.

-spec update_agent(position(), agent_state(), environment(), config()) ->
                          environment().
update_agent(Position, empty, Env, _Config) ->
    ets:delete(Env#env.agents, Position),
    Env;
update_agent(Position, NewState, Env, _Config) ->
    ets:insert(Env#env.agents, #agent{pos = Position, state = NewState}),
    Env.


-spec get_positions(agents(), tile()) -> [position()].
get_positions(Agents, Tile) ->
    {{F,_,_}, {T,_,_}} = Tile,
    Positions = ets:select(Agents, [{explicit_ant_record(),
                                     [{'and',
                                       {'>=', '$1', F},
                                       {'=<', '$1', T}}],
                                     [{{'$1', '$2', '$3'}}]}]),
    Positions.

%% fool dialyzer in 17.0
-spec explicit_ant_record() -> tuple().
explicit_ant_record() ->
    list_to_tuple([agent, {'$1', '$2', '$3'}, '_']).

-spec clean(atom()) -> ok | error.
clean(TableId) ->
    try ets:delete(TableId) of
        true -> ok
    catch
        _:_ -> error
    end.

-spec get_list(agents()) ->
                      [agent()].
get_list(Agents) ->
    ets:tab2list(Agents).


-spec get_tiles(pos_integer(), environment()) -> [{dimension(), tile()}].
get_tiles(Dist, Env) ->
    W = (Env#env.world)#world.w,
    TagTiles = [{I, Env#env.agents} || I <- lists:seq(1, W, Dist)],
    TagTiles.