-module(agents_lists).
-behaviour(agents).

-export([create_agents/3,
         get_agent/3,
         update_agent/4,
         get_positions/2,
         group_by/1,
         get_list/1,
         get_tiles/2,
         update_tiles/3]).

-export_type([agents/0]).

-type agent_state() :: parallant:agent_state().
-type agents() :: agents:agents([agent()]).
-type tile() :: agents:tile({Start :: dimension(), End :: dimension()}).

-include("parallant.hrl").

-spec create_agents(PopulationSize :: pos_integer(),
                    World :: world(),
                    config()) ->
                           agents().
create_agents(PopulationSize, World, Config) ->
    Pop = model:initial_population(PopulationSize, World, Config),
    [#agent{pos = Pos, state = State} || {Pos, State} <- Pop].

-spec get_agent(position(), environment(), config()) -> agent_state().
get_agent(Position, Env, _Config) ->
    Filtered = [State || #agent{pos = Pos, state = State} <- Env#env.agents,
                         Pos == Position],
    case Filtered of
        [] ->
            empty;
        [Agent] ->
            Agent
    end.

-spec update_agent(position(), agent_state(), environment(), config()) ->
                          environment().
update_agent(Position, empty, Env, _Config) ->
    Filtered = [A || A = #agent{pos = Pos} <- Env#env.agents, Pos /= Position],
    Env#env{agents = Filtered};
update_agent(Position, NewState, Env, Config) ->
    Update = fun (A = #agent{pos = Pos})
                   when Pos == Position ->
                     A#agent{state = NewState};
                 (A) -> A
             end,
    case get_agent(Position, Env, Config) of
        empty ->
            NewAgent = #agent{pos = Position, state = NewState},
            Env#env{agents = [NewAgent | Env#env.agents]};
        _ ->
            Env#env{agents = lists:map(Update, Env#env.agents)}
    end.

-spec get_positions(agents(), tile()) -> [position()].
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
    Agents.

-spec get_tiles(pos_integer(), environment()) -> [{dimension(), tile()}].
get_tiles(Dist, Env) ->
    W = (Env#env.world)#world.w,
    Zeros = [{I, []} || I <- lists:seq(1, W, Dist)],
    AssignTileToAgent = fun(A = #agent{pos={X, _, _}}) ->
                                ITile = trunc((X-1)/Dist)*Dist+1,
                                {ITile, [A]}
                        end,
    TiledAgents = lists:map(AssignTileToAgent, Env#env.agents),
    TagTiles = group_by(TiledAgents ++ Zeros),
    TagTiles.

-spec update_tiles({tile(), environment()}, environment(), config()) -> environment().
update_tiles(NewEnvs, Env, Config) ->
    ApplyEnv = mk_apply_env(Config),
    lists:foldl(ApplyEnv, Env, NewEnvs).

mk_apply_env(Config) ->
    fun({Tile, TileEnv}, EAcc) ->
            UpdateAgent =
                fun(Pos, EAcc2) ->
                        NewState = get_agent(Pos, TileEnv, Config),
                        update_agent(Pos, NewState, EAcc2, Config)
                end,
            Neighbours = agents:neighbourhood(Tile, TileEnv, Config),
            lists:foldl(UpdateAgent, EAcc, Neighbours)
    end.