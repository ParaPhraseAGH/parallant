%%%-------------------------------------------------------------------
%%% @author Daniel Grzonka
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% Ants gb_trees based approach
%%% @end
%%% Created : 07. lis 2014 15:56
%%%-------------------------------------------------------------------
-module(ants_gbt).
-author("Daniel").

%% API
-export([create_ants/4, apply_move/3, partition/3]).

-include("parallant.hrl").

%% TODO balancing tree

-spec create_ants(pos_integer(), dimension(), dimension(), config()) -> gb_trees:tree().
create_ants(PopulationSize, Width, Height, Config) ->
    AllPositions = [{I, J} || I <- lists:seq(1, Width),
                              J <- lists:seq(1, Height)],
    ShuffledCellPositions = shuffle(AllPositions),
    AntPositions = lists:sublist(ShuffledCellPositions, 1, PopulationSize),
    gb_trees:from_orddict([{Pos,
                            #ant{pos = Pos, state = model:random_ant_state(Config)}} ||
                              Pos <- lists:sort(AntPositions)]).


-spec apply_move({ant(), ant()}, environment(), config()) -> environment().
apply_move({Old, New}, E, Config) ->
    case gb_trees:lookup(New#ant.pos, E#env.agents) of
        none ->
            AgentsAfterInsert = gb_trees:insert(New#ant.pos, New, E#env.agents),
            AgentsAfterDelete = gb_trees:delete(Old#ant.pos, AgentsAfterInsert),
            update_cell(Old#ant.pos, E#env{agents = AgentsAfterDelete}, Config);
        {value, _} ->
            E
    end.

-spec update_cell(position(), environment(), config()) -> environment().
update_cell(Pos, E = #env{world = World}, Config) ->
    E#env{world = world_impl:update_cell(Pos, World, Config)}.


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


-spec partition(environment(), pos_integer(), pos_integer()) -> [[ant()]].
partition(Env, 1, 1) ->
    [gb_trees:values(Env#env.agents)]; %added: Conversion into list
partition(Env, NColours, NParts) ->
    W = (Env#env.world)#world.w,
    %% H = 5,
    D = round(W/NParts),
    Zeros = [{I, []} || I <- lists:seq(1, W, D)],
    AssignTileToAnt = fun(A = #ant{pos={X, _}}) ->
                              ITile = trunc((X-1)/D)*D+1,
                              {ITile, [A]}
                      end,
    TiledAnts = lists:map(AssignTileToAnt, gb_trees:values(Env#env.agents)), %added: Conversion into list
    TagTiles = group_by(TiledAnts ++ Zeros),
    Tiles = [T || {_, T} <- TagTiles],
    Colours = group_by_colour(Tiles, NColours),
    Colours.


-spec shuffle(list()) -> list().
shuffle(L) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].