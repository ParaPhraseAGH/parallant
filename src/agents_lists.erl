-module(agents_lists).
-behaviour(agents).

-export([create_agents/3,
         partition/3,
         get_agent/3,
         update_agent/4,
         get_positions/2,
         group_by/1]).

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

-spec partition(environment(),
                Colours :: pos_integer(),
                Parts :: pos_integer()) ->
                       [[{tile(), agents()}]].
partition(Env, 1, 1) ->
    [[{unique, Env#env.agents}]];
partition(Env, NColours, NParts) ->
    W = (Env#env.world)#world.w,
    %% H = 5,
    D = round(W/(NParts*NColours)),
    Zeros = [{I, []} || I <- lists:seq(1, W, D)],
    AssignTileToAgent = fun(A = #agent{pos={X, _, _}}) ->
                                ITile = trunc((X-1)/D)*D+1,
                                {ITile, [A]}
                        end,
    TiledAgents = lists:map(AssignTileToAgent, Env#env.agents),
    TagTiles = group_by(TiledAgents ++ Zeros),
    Tiles = [{{I, I+D-1}, T} || {I, T} <- TagTiles],
    Colours = group_by_colour(Tiles, NColours),
    Colours.

-spec get_positions(agents(), tile()) -> [position()].
get_positions(Agents, _Tile) ->
    [A#agent.pos || A <- Agents].

-spec group_by([{term(), [term()]}]) -> [{term(), [term()]}].
group_by(List) ->
    dict:to_list(
      lists:foldl(fun({K, V}, D) ->
                          dict:append_list(K, V, D)
                  end, dict:new(), List)).

-spec group_by_colour([agents()], pos_integer()) -> [agents()].
group_by_colour(Tiles, N) ->
    N = 2, % dividing in stripes
    EveryNth = fun (Rest) ->
                       [A || {I, A} <- lists:zip(lists:seq(1, length(Tiles)),
                                                 Tiles),
                             I rem N == Rest]
               end,
    lists:map(EveryNth, [I rem N || I <- lists:seq(1, N)]).
