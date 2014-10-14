%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2014 1:40 PM
%%%-------------------------------------------------------------------
-module(tiled_skel).
-author("piotr").

-include("parallant.hrl").

%% API
-export([split_ants_to_tiles/4, flatten_tiles/1, update_colours/7]).

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
    %%   io:format("Tiles: ~p ~n", [Tiles]),
    IndexedTiles = lists:zip(lists:seq(1, length(Tiles)), Tiles),
    %%   io:format("IndexedTiles: ~p ~n", [IndexedTiles]),
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
    %%   io:format("AntDictWithEmpty: ~p ~n", [dict:to_list(AntDictWithEmptyTiles)]),
    {AntDictWithEmptyTiles, TilesDict}.


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


-spec update_colours(KColours, Ants, TilesDict, Width, Height, Board, Impl) -> Ants when
      KColours :: pos_integer(),
      Ants :: dict:dict(pos_integer(), [ant()]),
      TilesDict :: dict:dict(pos_integer(), tile()),
      Width :: dimension(),
      Height :: dimension(),
      Board :: board(),
      Impl :: world_impl().

update_colours(KColours, Ants, TilesDict, Width, Height, Board, Impl) ->
                                                % new dict with empty lists for every key in ants
    EmptyMovedAnts = dict:map(fun(_K, _V) -> [] end, Ants),
    update_colours(1, KColours, Ants, EmptyMovedAnts, TilesDict, Width, Height, Board, Impl).


-spec update_colours(IColour, KColours, Ants, MovedAnts, TilesDict, Width, Height, Board, Impl) -> NewAnts when
      IColour :: pos_integer(),
      KColours :: pos_integer(),
      Ants :: dict:dict(pos_integer(), [ant()]),
      MovedAnts :: dict:dict(pos_integer(), [ant()]),
      TilesDict :: dict:dict(pos_integer(), tile()),
      Width :: dimension(),
      Height ::dimension(),
      Board :: board(),
      Impl :: world_impl(),
      NewAnts :: dict:dict(pos_integer(), [ant()]).

update_colours(IColour, KColours, _Ants, MovedAnts, _TilesDict, _Width, _Height, _Board, _Impl) when IColour == KColours + 1 ->
    %%   io:format("colours(IC:~p, KC:~p): MovedAnts: ~p ~n", [IColour, KColours, dict:to_list(MovedAnts)]),
    MovedAnts;
update_colours(IColour, KColours, Ants, MovedAnts, TilesDict, Width, Height, Board, Impl) ->
    %%   io:format("Colour: ~p~n",[IColour]),
    {Processed, JustMoved} = update_colour(IColour, KColours, Ants, MovedAnts, TilesDict, Width, Height, Board, Impl),
    %%   io:format("colours(): Processed : ~p, Ants: ~p ~n", [Processed, dict:to_list(Ants)]),
    %%   io:format("colours(): JustMoved: ~p ~n", [JustMoved]),
    NewAnts = update_ants(Processed, Ants), % clear ants tile if tile processed,
    %%   io:format("colours(): NewAnts: ~p ~n", [dict:to_list(NewAnts)]),
    NewMovedAnts = update_moved_ants(JustMoved, MovedAnts),
    %%   io:format("colours(): MovedAnts: ~p ~n", [dict:to_list(MovedAnts)]),
    %%   io:format("colours(): NewMovedAnts: ~p ~n", [dict:to_list(NewMovedAnts)]),
    update_colours(IColour + 1, KColours, NewAnts, NewMovedAnts, TilesDict, Width, Height, Board, Impl).


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


-spec update_colour(IColour, KColours, Ants, MovedAnts, TilesDict, Width, Height, Board, Impl) -> {Processed, MovedAntsWithIndices} when
      IColour :: pos_integer(),
      KColours :: pos_integer(),
      Ants :: dict:dict(pos_integer(),[ant()]),
      MovedAnts :: dict:dict(pos_integer(),[ant()]),
      TilesDict :: dict:dict(pos_integer(),[tile()]),
      Width :: dimension(),
      Height ::dimension(),
      Board :: board(),
      Impl :: world_impl(),
      Processed :: [pos_integer()],
      MovedAntsWithIndices :: [{pos_integer(), [ant()]}].

update_colour(IColour, KColours, Ants, MovedAnts, TilesDict, Width, Height, Board, Impl) ->
                                                % FIXME this part can be parallelized
    MaxI = dict:size(Ants),
    IndexTriplets = [[I, neighbour(left, I, MaxI), neighbour(right, I, MaxI)]
                     || I <- lists:seq(IColour, MaxI, KColours)],
    UpdateTileFun =  fun (T = [I, ILeft, IRight]) ->
                             {T, update_tile(
                                   dict:fetch(I, Ants),
                                   { dict:fetch(I, TilesDict),
                                     dict:fetch(ILeft, TilesDict)},
                                   dict:fetch(I, MovedAnts),
                                   merge_neighbourhood(ILeft, IRight, Ants, MovedAnts),
                                   Width,
                                   Height,
                                   Board,
                                   Impl)}
                     end,
    UpdateTile = {seq, UpdateTileFun},
    Zip = {seq, fun({Indices, MovedTiles}) -> lists:zip(Indices, MovedTiles) end},
    Pipe = {pipe, [UpdateTile, Zip]},
    TileFlow = {map,
                [Pipe],
                MaxI},
    Flatten = {seq, fun lists:flatten/1},
    GroupByConcat = {seq, fun group_by_concat/1},
    Workflow = {pipe, [TileFlow,
                       Flatten,
                       GroupByConcat]},
    [GrouppedMovedAntsWithIndices] = skel:do([Workflow], [IndexTriplets]),
    Processed = [hd(Triplet) || Triplet <- IndexTriplets],
    {Processed, GrouppedMovedAntsWithIndices}.

-spec update_tile(TileAnts, Tiles, MovedToTileAnts, NeighbourAnts, Width, Height, Board, Impl) ->
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
      Impl :: world_impl(),
      MovedTileAnts :: [ant()],
      MovedToLeftAnts :: [ant()],
      MovedToRightAnts :: [ant()].

update_tile([], _Tiles, _MovedToTileAnts, _Neighbours, _Width, _Height, _Board, _Impl) ->
    [[], [], []];
update_tile(TileAnts, Tiles, MovedToTileAnts, {LeftAnts, RightAnts}, Width, Height, Board, Impl) ->
                                                % turn and move every ant if relevant cell is not occupied by ants from MoveToTile, Left or Right
                                                % return moved ants split in three areas center, left neighbour, right neighbour
    OccuppiedCells = MovedToTileAnts ++ LeftAnts ++ RightAnts,
    GetAntCell = fun(#ant{pos = Pos}) -> parallant:get_cell(Impl, Pos, Width, Height, Board) end,
    AntCells = lists:map(GetAntCell, TileAnts),
    NewAnts = parallant:move_ants(AntCells, TileAnts, Width, Height, OccuppiedCells),
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
