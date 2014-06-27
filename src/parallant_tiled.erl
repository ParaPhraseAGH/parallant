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
-export([test/0, display/5, run/6]).

-include("parallant.hrl").

-spec test() -> ok.
test() ->
  parallant:test(?MODULE).

-spec run(world_impl(), [cell()], dimension(), dimension(), dict:dict(pos_integer(),[ant()]), pos_integer()) -> {[cell()],[ant()]}.
run(Impl, Board, Width, Height, Ants, Steps) ->
  KColours = 2,
  NWorkers = 2,
  NParts = NWorkers * KColours,
  {TiledAnts, TilesDict} = topo:split_ants_to_tiles(Ants, Width, Height, NParts),
  {EndBoard,EndTiledAnts} = step(Impl, Board, Width, Height, TiledAnts, TilesDict, 1, Steps),
  EndAnts = topo:flatten_tiles(EndTiledAnts),
  {EndBoard, EndAnts}.

-spec display(world_impl(), ant(), board(), dimension(), dimension()) -> ok.
display(Impl, Ants, Board, Width, Height) when is_list(Ants)->
  Impl:display(Ants, Board, Width, Height);
display(Impl, Ants, Board, Width, Height) ->
  FlattenedAnts = topo:flatten_tiles(Ants),
  Impl:display(FlattenedAnts, Board, Width, Height).

-spec step(Impl, Board, Width, Height, TiledAnts, TilesDict, CurrentStep, MaxStep) -> {EndAnts, EndBoard} when
  Impl :: world_impl(),
  Board :: board(),
  Width :: dimension(),
  Height :: dimension(),
  TiledAnts :: dict:dict(pos_integer(), [ant()]),
  TilesDict :: dict:dict(pos_integer(), tile()),
  CurrentStep :: pos_integer(),
  MaxStep :: pos_integer(),
  EndAnts :: dict:dict(pos_integer(), [ant()]),
  EndBoard :: board().

step(_Impl, Board, _Width, _Height, TiledAnts, _TilesDict, MaxStep, MaxStep) ->
  {Board, TiledAnts};
step(Impl, Board, Width, Height, Ants, TilesDict, Step, MaxStep) ->
  KColours = 2,
  NewAnts = topo:update_colours(KColours, Ants, TilesDict, Width, Height, Board, Impl),
  AntList = topo:flatten_tiles(Ants),
  NewBoard = parallant:update_board(Impl, Board, Width, Height, AntList),
  parallant:log(?MODULE, Impl, NewAnts, NewBoard, Step, Width, Height),
  step(Impl, NewBoard, Width, Height, NewAnts, TilesDict, Step+1, MaxStep).
