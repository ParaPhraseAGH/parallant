%%%-------------------------------------------------------------------
%%% @author Daniel Grzonka
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% Agents gb_trees based approach
%%% @end
%%% Created : 07. lis 2014 15:56
%%%-------------------------------------------------------------------
-module(agents_gbtree).
-behaviour(agents).
-author("Daniel").

%% API
-export([create_agents/3,
         get_agent/3,
         update_agent/4,
         get_positions/2,
         group_by/1,
         get_list/1,
         get_tiles/2]).

-include("parallant.hrl").

-type tile() :: agents:tile({Start :: dimension(), End :: dimension()}).
-type agent_state() :: parallant:agent_state().
-type agents() :: agents:agents(gb_trees:tree(position(), agent_state())).

-spec create_agents(PopulationSize :: pos_integer(),
                    World :: world(),
                    config()) ->
                           Agents :: agents().
create_agents(PopulationSize, World, Config) ->
    Pop = model:initial_population(PopulationSize, World, Config),
    IndividualsWithKeys = [{Pos,
                            #agent{pos = Pos, state = State}}
                           || {Pos, State} <- Pop],
    gb_trees:from_orddict(lists:sort(IndividualsWithKeys)).

-spec get_agent(position(), environment(), config()) -> agent_state().
get_agent(Position, Env, _Config) ->
    TreeRes = gb_trees:lookup(Position,  Env#env.agents),
    case TreeRes of
        {value, #agent{state = State}} ->
            State;
        none ->
            empty
    end.

-spec update_agent(position(), agent_state(), environment(), config()) ->
                          environment().
update_agent(Position, empty, Env, _Config) ->
    Env#env{agents = gb_trees:delete_any(Position, Env#env.agents)};
update_agent(Position, NewState, Env, Config) ->
    NewAgent = #agent{pos = Position, state = NewState},
    case get_agent(Position, Env, Config) of
        empty ->
            NewAgents = gb_trees:insert(Position, NewAgent, Env#env.agents),
            Env#env{agents = NewAgents};
        _ ->
            NewAgents = gb_trees:update(Position, NewAgent, Env#env.agents),
            Env#env{agents = NewAgents}
    end.

-spec get_positions(agents_lists:agents(), tile()) -> [position()].
get_positions(Agents, _Tile) ->
    [A#agent.pos || A <- Agents].

-spec group_by([{term(), [term()]}]) -> [{term(), [term()]}].
group_by(List) ->
    dict:to_list(
      lists:foldl(fun({K, V}, D) ->
                          dict:append_list(K, V, D)
                  end, dict:new(), List)).

-spec get_list(agents()) ->
                      [agent()].
get_list(Agents) ->
    gb_trees:to_list(Agents).

-spec get_tiles(pos_integer(), environment()) -> [{dimension(), tile()}].
get_tiles(Dist, Env) ->
    W = (Env#env.world)#world.w,
    Zeros = [{I, []} || I <- lists:seq(1, W, Dist)],
    AssignTileToAgent = fun(A = #agent{pos={X, _, _}}) ->
                                ITile = trunc((X-1)/Dist)*Dist+1,
                                {ITile, [A]}
                        end,
    TiledAgents = lists:map(AssignTileToAgent, get_list(Env#env.agents)),
    TagTiles = group_by(TiledAgents ++ Zeros),
    TagTiles.