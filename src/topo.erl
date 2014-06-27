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
-compile(export_all).
%% API
-export([split_ants_to_tiles/4, flatten_tiles/1, step/8]).

% NParts = NWorkers * NColors
-spec split_ants_to_tiles(Ants, Width, Height, NParts) -> {TiledAnts, TilesDict} when
  Ants :: [ant()],
  Width :: dimension(),
  Height :: dimension(),
  NParts :: pos_integer(),
  TiledAnts :: dict:dict(TileIndex, AntList),
  TilesDict :: dict:dict(TileIndex, tile()),
  TileIndex :: pos_integer(),
  AntList :: [ant()].

split_ants_to_tiles(Ants, Width, Height, NParts) ->
  D = round(Width/NParts),
  Tiles = [#tile{ min_x = I,
                  max_x = I + D - 1,
                  min_y = 1,
                  max_y = Height} || I <- lists:seq(1,Width, D)],
  io:format("Tiles: ~p ~n", [Tiles]),
  IndexedTiles = lists:zip(lists:seq(1, length(Tiles)), Tiles),
  io:format("IndexedTiles: ~p ~n", [IndexedTiles]),
%%   Groupped = group_by(IndexedTiles),
%%   io:format("Groupped: ~p ~n", [Groupped]),
  TilesDict = dict:from_list(IndexedTiles),
%%   io:format("TilesDict: ~p ~n", [TilesDict]),
  % map ants to indices of tiles
  TiledAnts = [find_ant_tile(A, IndexedTiles) || A <- Ants],
  EmptyTiles = [{I, []} || {I, _T} <- IndexedTiles],
%%   io:format("TiledAnts: ~p ~n", [TiledAnts]),
  AntDict = dict:from_list(group_by(TiledAnts)),
%%   io:format("AntDict: ~p ~n", [dict:to_list(AntDict)]),
  Concat = fun(_Key, Val1, Val2) -> Val1 ++ Val2 end,
  AntDictWithEmptyTiles =
    dict:merge(Concat, dict:from_list(EmptyTiles), AntDict),
  io:format("AntDictWithEmpty: ~p ~n", [dict:to_list(AntDictWithEmptyTiles)]),
  {AntDictWithEmptyTiles, TilesDict}.

find_ant_tile(_Ant, []) ->
%%   io:format("Ant: ~p couldnt find its tile~n",[_Ant]),
  undefined;
find_ant_tile(Ant, [{I, Tile} | Tiles]) ->
  case is_ant_on_tile(Ant, Tile) of
    true ->
%%       io:format("Ant: ~p found its tile: ~p~n", [Ant, Tile]),
      {I, Ant};
    false -> find_ant_tile(Ant, Tiles)
  end.

-spec flatten_tiles(TiledAnts) -> Ants when
  TiledAnts :: dict:dict(TileIndex, AntList),
  Ants :: [ant()],
  TileIndex :: pos_integer(),
  AntList :: [ant()].

flatten_tiles(Tiled) ->
  Flatten = fun (_K, Val, Acc) -> Val ++ Acc end,
  dict:fold(Flatten, [], Tiled).


-spec is_ant_on_tile(Ant, Tile) -> Result when
  Ant :: ant(),
  Tile :: tile(),
  Result :: boolean().

is_ant_on_tile(#ant{pos = {X, Y}}, T) when  X =< T#tile.max_x,
                                            X >= T#tile.min_x,
                                            Y >= T#tile.min_y,
                                            Y =< T#tile.max_y ->
  true;
is_ant_on_tile(_Ant, _Tile) ->
  false.


-spec step(Model, Board, Width, Height, TiledAnts, TilesDict, CurrentStep, MaxStep) -> {EndAnts, EndBoard} when
  Model :: model(),
  Board :: board(),
  Width :: dimension(),
  Height :: dimension(),
  TiledAnts :: dict:dict(pos_integer(), [ant()]),
  TilesDict :: dict:dict(pos_integer(), tile()),
  CurrentStep :: pos_integer(),
  MaxStep :: pos_integer(),
  EndAnts :: dict:dict(pos_integer(), [ant()]),
  EndBoard :: board().

step(_Model, Board, _Width, _Height, TiledAnts, _TilesDict, MaxStep, MaxStep) ->
  {TiledAnts, Board};
step(Model, Board, Width, Height, Ants, TilesDict, Step, MaxStep) ->
  KColours = 2,
%%   io:format("step(~p): Ants: ~p ~n", [Step, dict:to_list(Ants)]),
  NewAnts = update_colours(KColours, Ants, TilesDict, Width, Height, Board, Model),
%%   io:format("step(~p): NewAnts: ~p ~n", [Step, dict:to_list(NewAnts)]),
  NewBoard = update_board(Model, Board, Width, Height, Ants),
  parallant_tiled:log(Model, NewAnts, NewBoard, Step, Width, Height),
  step(Model, NewBoard, Width, Height, NewAnts, TilesDict, Step+1, MaxStep).

update_board(Model, Board, W, H, Ants) ->
  parallant_tiled:update_board(Model, Board, W, H, Ants).


-spec update_colours(KColours, Ants, TilesDict, Width, Height, Board, Model) -> Ants when
  KColours :: pos_integer(),
  Ants :: dict:dict(pos_integer(), [ant()]),
  TilesDict :: dict:dict(pos_integer(), tile()),
  Width :: dimension(),
  Height :: dimension(),
  Board :: board(),
  Model :: model().

update_colours(KColours, Ants, TilesDict, Width, Height, Board, Model) ->
  % new dict with empty lists for every key in ants
  EmptyMovedAnts = dict:map(fun(_K, _V) -> [] end, Ants),
  update_colours(1, KColours, Ants, EmptyMovedAnts, TilesDict, Width, Height, Board, Model).


-spec update_colours(IColour, KColours, Ants, MovedAnts, TilesDict, Width, Height, Board, Model) -> NewAnts when
  IColour :: pos_integer(),
  KColours :: pos_integer(),
  Ants :: dict:dict(pos_integer(), [ant()]),
  MovedAnts :: dict:dict(pos_integer(), [ant()]),
  TilesDict :: dict:dict(pos_integer(), tile()),
  Width :: dimension(),
  Height ::dimension(),
  Board :: board(),
  Model :: model(),
  NewAnts :: dict:dict(pos_integer(), [ant()]).

update_colours(IColour, KColours, _Ants, MovedAnts, _TilesDict, _Width, _Height, _Board, _Model) when IColour == KColours + 1 ->
%%   io:format("colours(IC:~p, KC:~p): MovedAnts: ~p ~n", [IColour, KColours, dict:to_list(MovedAnts)]),
  MovedAnts;
update_colours(IColour, KColours, Ants, MovedAnts, TilesDict, Width, Height, Board, Model) ->
%%   io:format("Colour: ~p~n",[IColour]),
  {Processed, JustMoved} = update_colour(IColour, KColours, Ants, MovedAnts, TilesDict, Width, Height, Board, Model),
%%   io:format("colours(): Processed : ~p, Ants: ~p ~n", [Processed, dict:to_list(Ants)]),
%%   io:format("colours(): JustMoved: ~p ~n", [JustMoved]),
  NewAnts = update_ants(Processed, Ants), % clear ants tile if tile processed,
%%   io:format("colours(): NewAnts: ~p ~n", [dict:to_list(NewAnts)]),
  NewMovedAnts = update_moved_ants(JustMoved, MovedAnts),
%%   io:format("colours(): MovedAnts: ~p ~n", [dict:to_list(MovedAnts)]),
%%   io:format("colours(): NewMovedAnts: ~p ~n", [dict:to_list(NewMovedAnts)]),
  update_colours(IColour + 1, KColours, NewAnts, NewMovedAnts, TilesDict, Width, Height, Board, Model).


-spec update_moved_ants(JustMoved, MovedAnts) -> NewMovedAnts when
  JustMoved :: [{TileIndex, AntList}],
  MovedAnts :: dict:dict(TileIndex, AntList),
  NewMovedAnts :: dict:dict(TileIndex, AntList),
  TileIndex :: pos_integer(),
  AntList :: [ant()].

update_moved_ants(JustMoved, MovedAnts) ->
  % update movedants with what whas just moved
  dict:merge(fun (_K, V, V2) -> lists:append(V, V2) end, dict:from_list(JustMoved), MovedAnts).


-spec update_ants(Processed, Ants) -> NewAnts when
  Processed :: [TileIndex],
  Ants :: dict:dict(TileIndex, AntList),
  NewAnts :: dict:dict(TileIndex, AntList),
  TileIndex :: pos_integer(),
  AntList :: [ant()].

update_ants(Processed, Ants) ->
  % delete ants that were just processed
  Filter = fun(Key, Value) ->
              case lists:member(Key, Processed) of
                true -> [];
                false -> Value
              end
           end,
  dict:map(Filter, Ants).


-spec update_colour(IColour, KColours, Ants, MovedAnts, TilesDict, Width, Height, Board, Model) -> {Processed, MovedAntsWithIndices} when
  IColour :: pos_integer(),
  KColours :: pos_integer(),
  Ants :: dict:dict(pos_integer(),[ant()]),
  MovedAnts :: dict:dict(pos_integer(),[ant()]),
  TilesDict :: dict:dict(pos_integer(),[tile()]),
  Width :: dimension(),
  Height ::dimension(),
  Board :: board(),
  Model :: model(),
  Processed :: [pos_integer()],
  MovedAntsWithIndices :: [{pos_integer(), [ant()]}].

update_colour(IColour, KColours, Ants, MovedAnts, TilesDict, Width, Height, Board, Model) ->
  % FIXME this part can be parallelized
  MaxI = dict:size(Ants),
%%   io:format("MAXI = ~p = SIZE(ANTS = ~p)~n",[MaxI, dict:to_list(Ants)]),
  IndexTriplets = [[I, neighbour(left, I, MaxI), neighbour(right, I, MaxI)]
    || I <- lists:seq(IColour, MaxI, KColours)],
  MovedTuples = [
    {T, update_tile(
          dict:fetch(I, Ants),
          { dict:fetch(I, TilesDict),
            dict:fetch(ILeft, TilesDict)},
          dict:fetch(I, MovedAnts),
          merge_neighbourhood(ILeft, IRight, Ants, MovedAnts),
          Width,
          Height,
          Board,
          Model)
    } ||
    T = [I, ILeft, IRight] <- IndexTriplets
  ],

  % merge results and return
  MovedAntsWithIndices = lists:flatten([lists:zip(Indices, MovedTiles) || {Indices, MovedTiles} <- MovedTuples]),
  Processed = [I || [I, _Left, _Right] <- IndexTriplets],
%%   {Processed, MovedAntsWithIndices}.
  {Processed, group_by_concat(MovedAntsWithIndices)}.

-spec update_tile(TileAnts, Tiles, MovedToTileAnts, NeighbourAnts, Width, Height, Board, Model) ->
  {MovedTileAnts, MovedToLeftAnts, MovedToRightAnts} when
  TileAnts :: [ant()],
  Tiles :: {Tile, LeftTile},
  Tile :: tile(),
  LeftTile :: tile(),
  MovedToTileAnts :: [ant()], % readonly
  NeighbourAnts :: {LeftNeighbourAnts, RightNeighbourAnts},
  LeftNeighbourAnts :: [ant()], % readonly
  RightNeighbourAnts :: [ant()], % readonly
  Width :: dimension(),
  Height ::dimension(),
  Board :: board(), % readonly
  Model :: model(),
  MovedTileAnts :: [ant()],
  MovedToLeftAnts :: [ant()],
  MovedToRightAnts :: [ant()].

update_tile([], _Tiles, _MovedToTileAnts, _Neighbours, _Width, _Height, _Board, Model) ->
  [[], [], []];
update_tile(TileAnts, Tiles, MovedToTileAnts, {LeftAnts, RightAnts}, Width, Height, Board, Model) ->
  % turn and move every ant if relevant cell is not occupied by ants from MoveToTile, Left or Right
  % return moved ants split in three areas center, left neighbour, right neighbour
  OccuppiedCells = MovedToTileAnts ++ LeftAnts ++ RightAnts,
  GetAntCell = fun(#ant{pos = Pos}) -> parallant_tiled:get_cell(Model, Pos, Width, Height, Board) end,
  AntCells = lists:map(GetAntCell, TileAnts),
  NewAnts = parallant_tiled:move_ants(AntCells, TileAnts, Width, Height, OccuppiedCells),
  SplittedAnts = split_moved_ants(NewAnts, Tiles),
  SplittedAnts.

split_moved_ants(NewAnts, Tiles) ->
  % if on tile, append to first one, if on left, append to secodn one, if on right append to third one
  split_moved_ants(NewAnts, Tiles, {[], [], []}).

-spec split_moved_ants(TileAnts, Tiles, Accumulators) -> Accumulators when
  TileAnts :: [ant()],
  Tiles :: {Tile, LeftTile},
  Tile :: tile(),
  LeftTile :: tile(),
  Accumulators :: {[ant()], [ant()], [ant()]}.

split_moved_ants([], _Tiles, Accs) ->
  tuple_to_list(Accs);
split_moved_ants([A | Ants], Tiles = {Tile, LeftTile}, {Acc, LeftAcc, RightAcc}) ->
  case is_ant_on_tile(A, Tile) of
    true -> split_moved_ants(Ants, Tiles, {[A | Acc], LeftAcc, RightAcc});
    false -> case is_ant_on_tile(A, LeftTile) of
               true -> split_moved_ants(Ants, Tiles, {Acc, [A | LeftAcc], RightAcc});
               false ->
%%                  io:format("ant: ~p on right tile. center ~p and left ~p ~n", [A, Tile, LeftTile]),
                 split_moved_ants(Ants, Tiles, {Acc, LeftAcc, [A | RightAcc]})
             end
  end.

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


neighbour(left, I, _N) when 1 < I -> I - 1;
neighbour(left, I, N) when I == 1 -> N;
neighbour(right, I, N) when I < N -> I + 1;
neighbour(right, I, N) when I == N -> 1.

merge_ant_lists(A, MA) -> A ++ MA.

%% %% [{K1,V1},{K2,V2},...] -> [{K1,[V1 , V3]},{K2,[V2 , V4 , V5]},...]
-spec group_by([{Index, [A]}]) -> [{Index, [A]}] when
  Index :: pos_integer(),
  A :: term().

group_by(List) ->
  dict:to_list(
    lists:foldl(fun({K,V}, D) ->
      dict:append(K, V, D)
    end, dict:new(), List)).


%% %% [{K1,V1},{K2,V2},...] -> [{K1,[V1 ++ V3]},{K2,[V2 ++ V4 ++ V5]},...]
-spec group_by_concat([{Index, [A]}]) -> [{Index, [A]}] when
  Index :: pos_integer(),
  A :: term().

group_by_concat(List) ->
  dict:to_list(
    lists:foldl(fun({K,V}, D) ->
      dict:append_list(K, V, D)
    end, dict:new(), List)).