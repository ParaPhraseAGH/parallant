-module(ants).

-include("parallant.hrl").

-behaviour(ants_impl).

-export([create_ants/4, apply_move/3, partition/3]).

-spec create_ants(PopulationSize :: pos_integer(),
                  Width :: dimension(),
                  Height :: dimension(),
                  config()) ->
                         [ant()].
create_ants(PopulationSize, Width, Height, Config) ->
    AllPositions = [{I, J} || I <- lists:seq(1, Width),
                              J <- lists:seq(1, Height)],
    ShuffledCellPositions = shuffle(AllPositions),
    AntPositions = lists:sublist(ShuffledCellPositions, 1, PopulationSize),
    [#ant{pos = Pos, state = model:random_ant_state(Config)}
     || Pos <- AntPositions].

-spec apply_move({ant(), ant()}, environment(), config()) -> environment().
apply_move({Old, New}, E, Config) ->
    IsPosTaken = fun(#ant{pos = P}) -> P == New#ant.pos end,
    case lists:any(IsPosTaken, E#env.agents) of
        true ->
            E;
        false ->
            NewAgents = [A || A <- E#env.agents, A#ant.pos /= Old#ant.pos],
            update_cell(Old#ant.pos, E#env{agents = [New | NewAgents]}, Config)
    end.

-spec update_cell(position(), environment(), config()) -> environment().
update_cell(Pos, E = #env{world = World}, Config) ->
    E#env{world = world_impl:update_cell(Pos, World, Config)}.


-spec partition(environment(), pos_integer(), pos_integer()) -> [[ant()]].
partition(Env, 1, 1) ->
    [Env#env.agents];
partition(Env, NColours, NParts) ->
    W = (Env#env.world)#world.w,
    %% H = 5,
    D = round(W/NParts),
    Zeros = [{I, []} || I <- lists:seq(1, W, D)],
    AssignTileToAnt = fun(A = #ant{pos={X, _}}) ->
                              ITile = trunc((X-1)/D)*D+1,
                              {ITile, [A]}
                      end,
    TiledAnts = lists:map(AssignTileToAnt, Env#env.agents),
    TagTiles = group_by(TiledAnts ++ Zeros),
    Tiles = [T || {_, T} <- TagTiles],
    Colours = group_by_colour(Tiles, NColours),
    Colours.

-spec group_by([{term(), [term()]}]) -> [{term(), [term()]}].
group_by(List) ->
    dict:to_list(
      lists:foldl(fun({K, V}, D) ->
                          dict:append_list(K, V, D)
                  end, dict:new(), List)).

-spec group_by_colour([[ant()]], pos_integer()) -> [[ant()]].
group_by_colour(Tiles, N) ->
    N = 2, % dividing in stripes
    EveryNth = fun (Rest) ->
                       [A || {I, A} <- lists:zip(lists:seq(1, length(Tiles)),
                                                 Tiles),
                             I rem N == Rest]
               end,
    lists:map(EveryNth, [I rem N || I <- lists:seq(1, N)]).


-spec shuffle(list()) -> list().
shuffle(L) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].
