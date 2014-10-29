%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(parallant_tiled).
%% API
-export([test/0, display/1, run/2]).

-include("parallant.hrl").

-spec test() -> ok.
test() ->
    parallant:test(?MODULE).

-spec display(environment()) -> ok.
display(E = #env{agents = Ants}) when is_list(Ants) ->
    (E#env.backend):display(Ants, E#env.world).

-spec run(pos_integer(), environment()) -> environment().
run(Steps, Env) ->
    step(1, Steps, Env).

-spec step(pos_integer(), pos_integer(), environment()) -> environment().
step(MaxT, MaxT, Env) ->
    Env;
step(T, MaxT, Env) ->
    Partitioned = partition(Env),

    ProcessTile =
        fun(Tile, {Occupied, E}) ->
                F = fun (A) -> parallant:get_moves(E#env{agents = A}) end,
                Moves = F(Tile),
                %% Moves = lists:flatmap(F, Tile),

                parallant:apply_moves(Moves, E, Occupied)
        end,
    {NewOccupied, UpdatedEnv} = lists:foldl(ProcessTile,
                                         {Env#env.agents, Env},
                                         Partitioned),
    NewEnv = UpdatedEnv#env{agents = NewOccupied},
    logger:log(NewEnv),
    step(T+1, MaxT, NewEnv).


-spec partition(environment()) -> [[[ant()]]].
partition(Env) ->
    W = (Env#env.world)#world.w,
    %% H = 5,
    NStripes = 2,
    NColours = 2,
    D = round(W/NStripes),
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
                       lists:flatten(
                         [A || {I, A} <- lists:zip(lists:seq(1, length(Tiles)),
                                                   Tiles),
                               I rem N == Rest])
               end,
    lists:map(EveryNth, [I rem N || I <- lists:seq(1, N)]).
