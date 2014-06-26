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


update(MaxStep, MaxStep, Ants, Board) ->
  {Ants, Board};
update(Step, MaxStep, Ants, Board) ->
  KColours = 2,
  NewAnts = update_colours(KColours, Ants, Board),
  NewBoard = update_board(Ants, Board),
  update(Step+1, MaxStep, NewAnts, NewBoard).

update_board(Ants, Board) ->
  error(not_implemented).


-spec update_colours(KColours, Ants, Board) -> Ants when
  KColours :: pos_integer(),
  Ants :: dict:dict(pos_integer(), [ant()]),
  Board :: board().

update_colours(KColours, Ants, Board) ->
  MovedAnts = [], % TODO new dict with empty lists for every key in ants
  update_colours(1, KColours, Ants, MovedAnts, Board).


-spec update_colours(IColour, KColours, Ants, MovedAnts, Board) -> NewAnts when
  IColour :: pos_integer(),
  KColours :: pos_integer(),
  Ants :: dict:dict(pos_integer(), [ant()]),
  MovedAnts :: dict:dict(pos_integer(), [ant()]),
  Board :: board(),
  NewAnts :: dict:dict(pos_integer(), [ant()]).

update_colours(IColour, KColours, _Ants, MovedAnts, _Board) when IColour == KColours - 1 ->
  MovedAnts;
update_colours(IColour, KColours, Ants, MovedAnts, Board) ->
  {Processed, JustMoved} = update_colour(IColour, KColours, Ants, MovedAnts, Board),
  NewAnts = update_ants(Processed, Ants), % clear ants tile if tile processed,
  NewMovedAnts = update_moved_ants(JustMoved, MovedAnts),
  update_colours(IColour + 1, KColours, NewAnts, NewMovedAnts, Board).


-spec update_moved_ants(JustMoved, MovedAnts) -> NewMovedAnts when
  JustMoved :: [{TileIndex, AntList}],
  MovedAnts :: dict:dict(TileIndex, AntList),
  NewMovedAnts :: dict:dict(TileIndex, AntList),
  TileIndex :: pos_integer(),
  AntList :: [ant()].

update_moved_ants(JustMoved, MovedAnts) ->
  % TODO
  % update movedants with what whas just moved
  error(not_implemented).


-spec update_ants(Processed, Ants) -> NewAnts when
  Processed :: [TileIndex],
  Ants :: dict:dict(TileIndex, AntList),
  NewAnts :: dict:dict(TileIndex, AntList),
  TileIndex :: pos_integer(),
  AntList :: [ant()].

update_ants(Processed, Ants) ->
  % TODO
  % delete ants that were just processed
  error(not_implemented).


-spec update_colour(IColour, KColours, Ants, MovedAnts, Board) -> {Processed, MovedAntsWithIndices} when
  IColour :: pos_integer(),
  KColours :: pos_integer(),
  Ants :: dict:dict(pos_integer(),[ant()]),
  MovedAnts :: dict:dict(pos_integer(),[ant()]),
  Board :: board(),
  Processed :: [pos_integer()],
  MovedAntsWithIndices :: [{pos_integer(), [ant()]}].

update_colour(IColour, KColours, Ants, MovedAnts, Board) ->
%%   N = length(Ants),

%%   I = 1, % 1+K, 1+2K, ...
%%   ILeft = neighbour(left, I, N),
%%   IRight = neighbour(right, I, N),

%%   AntsLeft = merge(Ants[ILeft], MovedAnts[ILeft]),
%%   AntsRight = merge(Ants[IRight], MovedAnts[IRight]),
%%   {AntsLeft, AntsRight} = merge_neighbourhood(ILeft, IRight, Ants, MovedAnts),

%%   TileAnts = Ants[I],
%%   MovedToTileAnts = MovedAnts[I],
  %% MovedToList = [{I, ListOfAntsMovedToTileI}, {J, ListOFAntsMOvedToTileJ}, ...]
%%   MovedToList = update_tile(TileAnts, MovedToTileAnts, {AntsLeft, AntsRight}, Board),
%%   Processed = [1, 1+K, 1+2*K, ...]
%%   delete(I, Ants) % moved all ants

  % this part can be parallelized
  MaxI = length(Ants),
  IndexTriplets = [{I, neighbour(left, I, MaxI), neighbour(right, I, MaxI)}
    || I <- lists:seq(IColour, length(Ants), KColours)],
  MovedTuples = [
    {T, update_tile(
          dict:fetch(I, Ants),
          dict:fetch(I, MovedAnts),
          merge_neighbourhood(ILeft, IRight, Ants, MovedAnts),
          Board)
    } ||
    T = {I, ILeft, IRight} <- IndexTriplets
  ],

  % merge results and return
  MovedAntsWithIndices = lists:flatten([lists:zip(Indices, MovedTiles) || {Indices, MovedTiles} <- MovedTuples]),
  Processed = [I || {I, _Left, _Right} <- IndexTriplets],
  {Processed, group_by(MovedAntsWithIndices)}.

-spec update_tile(TileAnts, MovedToTileAnts, NeighbourAnts, Board) ->
  {MovedTileAnts, MovedToLeftAnts, MovedToRightAnts} when
  TileAnts :: [ant()],
  MovedToTileAnts :: [ant()], % readonly
  NeighbourAnts :: {LeftNeighbourAnts, RightNeighbourAnts},
  LeftNeighbourAnts :: [ant()], % readonly
  RightNeighbourAnts :: [ant()], % readonly
  Board :: board(), % readonly
  MovedTileAnts :: [ant()],
  MovedToLeftAnts :: [ant()],
  MovedToRightAnts :: [ant()].

update_tile(TileAnts, MovedToTileAnts, {LeftAnts, RightAnts}, Board) ->
  % TODO implement
  % turn and move every ant if relevant cell is not occupied by ants from MoveToTile, Left or Right
  % return moved ants split in three areas center, left neighbour, right neighbour
  {[], [], []}.

-spec merge_neighbourhood(IndexLeft, IndexRight, Ants, MovedAnts) -> {LeftAnts, RightAnts} when
  IndexLeft :: pos_integer(),
  IndexRight :: pos_integer(),
  Ants :: [ant()],
  MovedAnts :: [ant()],
  LeftAnts :: [ant()],
  RightAnts :: [ant()].

merge_neighbourhood(ILeft, IRight, Ants, MovedAnts) ->
  {
    merge_ant_lists(dict:fetch(ILeft, Ants), dict:fetch(ILeft, MovedAnts)),
    merge_ant_lists(dict:fetch(IRight, Ants), dict:fetch(IRight, MovedAnts))
  }.

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

merge_ant_lists(A, MA) -> A ++ MA.

%% [{K1,V1},{K2,V2},...] -> [{K1,[V1 ++ V3]},{K2,[V2 ++ V4 ++ V5]},...]
-spec group_by([{Index, [A]}]) -> [{Index, [A]}] when
  Index :: pos_integer(),
  A :: term().

group_by(List) ->
  dict:to_list(
    lists:foldl(fun({K,V}, D) ->
      dict:append_list(K, V, D)
    end, dict:new(), List)).