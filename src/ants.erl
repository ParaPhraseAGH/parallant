-module(ants).
-behaviour(ants_impl).

-export([create_ants/4,
         partition/3,
         get_agent/3,
         update_agent/4,
         group_by/1]).

-type ant_state() :: parallant:ant_state().
-type ants() :: ants_impl:ants([ant()]).
-type tile() :: ants_impl:tile({Start :: dimension(), End :: dimension()}).

-include("parallant.hrl").

-spec create_ants(PopulationSize :: pos_integer(),
                  Width :: dimension(),
                  Height :: dimension(),
                  config()) ->
                         ants().
create_ants(PopulationSize, Width, Height, Config) ->
    Pop = model:initial_population(PopulationSize, Width, Height, Config),
    [#ant{pos = Pos, state = State} || {Pos, State} <- Pop].

-spec get_agent(position(), environment(), config()) -> ant_state().
get_agent(Position, Env, _Config) ->
    Filtered = [State || #ant{pos = Pos, state = State} <- Env#env.agents,
                         Pos == Position],
    case Filtered of
        [] ->
            empty;
        [Agent] ->
            Agent
    end.

-spec update_agent(position(), ant_state(), environment(), config()) ->
                          environment().
update_agent(Position, empty, Env, _Config) ->
    Filtered = [A || A = #ant{pos = Pos} <- Env#env.agents, Pos /= Position],
    Env#env{agents = Filtered};
update_agent(Position, NewState, Env, Config) ->
    Update = fun (A = #ant{pos = Pos})
                   when Pos == Position ->
                     A#ant{state = NewState};
                 (A) -> A
             end,
    case get_agent(Position, Env, Config) of
        empty ->
            NewAgent = #ant{pos = Position, state = NewState},
            Env#env{agents = [NewAgent | Env#env.agents]};
        _ ->
            Env#env{agents = lists:map(Update, Env#env.agents)}
    end.

-spec partition(environment(), pos_integer(), pos_integer()) ->
                       [[{tile(), [position()]}]].
partition(Env, 1, 1) ->
    [[{unique, [Pos || #ant{pos = Pos} <- Env#env.agents]}]];
partition(Env, NColours, NParts) ->
    W = (Env#env.world)#world.w,
    %% H = 5,
    D = round(W/NParts),
    Zeros = [{I, []} || I <- lists:seq(1, W, D)],
    AssignTileToAnt = fun(#ant{pos = {X, _} = Pos}) ->
                              ITile = trunc((X-1)/D)*D+1,
                              {ITile, [Pos]}
                      end,
    TiledAnts = lists:map(AssignTileToAnt, Env#env.agents),
    TagTiles = group_by(TiledAnts ++ Zeros),
    Tiles = [{{I, I+D-1}, Tile} || {I, Tile} <- TagTiles],
    Colours = group_by_colour(Tiles, NColours),
    Colours.

-spec group_by([{term(), [term()]}]) -> [{term(), [term()]}].
group_by(List) ->
    dict:to_list(
      lists:foldl(fun({K, V}, D) ->
                          dict:append_list(K, V, D)
                  end, dict:new(), List)).

-spec group_by_colour([ants()], pos_integer()) -> [ants()].
group_by_colour(Tiles, N) ->
    N = 2, % dividing in stripes
    EveryNth = fun (Rest) ->
                       [A || {I, A} <- lists:zip(lists:seq(1, length(Tiles)),
                                                 Tiles),
                             I rem N == Rest]
               end,
    lists:map(EveryNth, [I rem N || I <- lists:seq(1, N)]).
