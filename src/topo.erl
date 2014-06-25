%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2014 1:40 PM
%%%-------------------------------------------------------------------
-module(topo).
-author("piotr").

-include("parallant.hrl").

%% API
-export([]).

% NParts = NWorkers * NColors
%% divide(Ants, W, NParts) ->


-spec update_tile(
    TileAnts :: [ant()],
    MovedToTileAnts :: [ant()], % readonly
    LeftNeighbourAnts :: [ant()], % readonly
    RightNeighbourAnts :: [ant()], % readonly
    Board :: board() % readonly
    ) ->
      {
        MovedTileAnts :: [ant()],
        MovedToLeftAnts :: [ant()],
        MovedToRightAnts :: [ant()]
      }.
update_tile(TileAnts, MovedToTileAnts, LeftNAnts, RightNAnts, Board) ->
  ok.

-spec perform_step(
    Tiles :: [tile()],
    Board :: board()
  ) -> {Tiles, Board}.
perform_step(Tiles, Board) ->
  % update n, co k-tych tile'i
  % zaczynajac od 1, konczac na n
  TileI = 2,
  update_tile(lists:nth(TileI, Tiles)),
  ok.

update_color(IColor, MovedToTiles, KColors, Tiles, Board) ->
  MaxI = length(Tiles),
  IndexTriplets = [{I, neighbour(left, I, MaxI), neighbour(right, I, MaxI)}
    || I <- lists:seq(IColor, length(Tiles), KColors)],
  Moved = [
    {T, update_tile(
          dict:fetch(I, Tiles),
          dict:fetch(I, MovedToTiles),
          dict:fetch(ILeft, Tiles),
          dict:fetch(IRight, Tiles),
          Board
        )} ||
    T = {I, ILeft, IRight} <- IndexTriplets
  ],
  Zipped = [lists:zip(Indices, MovedTiles) || {Indices, MovedTiles} <- Moved],
  group_by(Zipped).

has_ant_on_pos(Pos, Ants) ->
  IsAntOnPos = eq_ant_pos(Pos),
  lists:any(IsAntOnPos, Ants).

eq_ant_pos(KeyPos) ->
  fun (_Ant = {Pos, _D}) ->
    Pos == KeyPos
  end.

neighbour(left, I, _N) when 1 < I -> I - 1;
neighbour(left, I, N) when I == 1 -> N;
neighbour(right, I, N) when I < N -> I + 1;
neighbour(right, I, N) when I == N -> 1.


%% [{K1,V1},{K2,V2},...] -> [{K1,[V1 ++ V3]},{K2,[V2 ++ V4 ++ V5]},...]
-spec group_by([{term(),term()}]) -> groups().
group_by(List) ->
  dict:to_list(
    lists:foldl(fun({K,V}, D) ->
      dict:append_list(K, V, D)
    end, dict:new(), List)).