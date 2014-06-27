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
-export([start/4, test/0, start/5]).
-export([update_cell/1]).

-compile(export_all).

-include("parallant.hrl").
-define(LOG_DELAY, 50). % ms
-define(MAX_WIDTH_TO_SHOW, 65).

-spec test() -> ok.
test() ->
  Seed = erlang:now(),
  {Width, Height, NAnts, Steps} = {50, 30, 5, 500},
  io:format("ListBased:~n"),
  random:seed(Seed),
  start(list_based, Width, Height, NAnts, Steps),
  io:format("Gb_treeBased:~n"),
  random:seed(Seed),
  start(gbtree_based, Width, Height, NAnts, Steps).

-spec start(model(), dimension(), dimension(), pos_integer()) -> ok.
start(Model, Width, Height, Steps) ->
  start(Model, Width, Height, 1, Steps).

-spec start(model(), dimension(), dimension(), pos_integer(), pos_integer()) -> ok.
start(Model, Width, Height, PopulationSize, Steps) ->
  Board = create_board(Model, Width, Height),
  Ants = create_ants(Model, PopulationSize, Width, Height),

  if
    Width < ?MAX_WIDTH_TO_SHOW ->
      io:format("Ants: ~p~n", [Ants]),
      io:format("Step 1:~n"),
      Model:display(Ants, Board, Width, Height);
    true -> ok
  end,
  T1 = erlang:now(),

  KColours = 2,
  NWorkers = 2,
  NParts = NWorkers * KColours,
  {TiledAnts, TilesDict} = topo:split_ants_to_tiles(Ants, Width, Height, NParts),

  {EndTiledAnts,EndBoard} = topo:step(Model, Board, Width, Height, TiledAnts, TilesDict, 1, Steps),

  T2 = erlang:now(),

%%   io:format("EndAnts: ~p~n", [EndTiledAnts]),

  EndAnts = topo:flatten_tiles(EndTiledAnts),

  if
    Width < ?MAX_WIDTH_TO_SHOW ->
      io:format("Step ~p:~n", [Steps]),
      Model:display(EndAnts, EndBoard, Width, Height);
    true -> ok
  end,

  Time = timer:now_diff(T2, T1),
  TimeInSecs = Time / 1000000,
  io:format("Time elapsed: ~p. Time per iteration: ~p s~n", [TimeInSecs, TimeInSecs / Steps]).

%% -spec step(model(), [cell()], dimension(), dimension(), dict:dict(pos_integer(),[ant()]), pos_integer(), pos_integer()) -> {[cell()],[ant()]}.
%% step(_Model, Board, _W, _H, Ants, MaxT, MaxT) -> {Board, Ants};
%% step(Model, Board, W, H, Ants, T, MaxT) ->
%%   % FIXME co tu jest potrzebne do move_ants
%%   step(Model, NewBoard, W, H, NewAnts, T + 1, MaxT).

-ifdef(dont_override_disp).

override_display(_) ->
  ok.

-else.

override_display(Height) ->
  io:format("\033[~pA", [Height + 2]). % display in the same place as the previous step

-endif.

-ifdef(debug).

log(Model, NewAnts, NewBoard, Step, Width, Height) ->
%%  lists:map(fun({_,NewADir}) -> io:format("new ant dir ~p~n",[NewADir]) end,NewAnts),
%%  lists:map(fun({NewAPos,_}) -> io:format("new ant pos ~p~n",[NewAPos]) end,NewAnts),
  io:format("Step ~p:~n", [Step + 1]),
  NewFlattenedAnts = topo:flatten_tiles(NewAnts),
  Model:display(NewFlattenedAnts, NewBoard, Width, Height),
  timer:sleep(?LOG_DELAY),
  override_display(Height).

-else.
log(_,_,_,_,_,_) ->
  ok.
-endif.

-spec create_ants(pos_integer(), dimension(), dimension()) -> [ant()].
create_ants(PopulationSize, Width, Height) ->
  ShuffledCellPositions = util:shuffle(util:all_positions(Width, Height)),
  AntPositions = lists:sublist(ShuffledCellPositions, 1, PopulationSize),
  [#ant{pos = Pos, dir = util:random_direction()} || Pos <- AntPositions].

-spec move_ants([cell()], [ant()], dimension(), dimension(), position()) -> [ant()].
move_ants([], [], _, _, _) -> [];
move_ants([AntCell | TAntCells], [Ant | TAnts], W, H, Occuppied) ->
  NewAnt = move_ant(AntCell, Ant, W, H, Occuppied),
  [NewAnt | move_ants(TAntCells, TAnts, W, H, [NewAnt#ant.pos | Occuppied])].

update_board(Model, Board, W, H, Ants) ->
  Flattened = topo:flatten_tiles(Ants),
  Model:update_board(Board, W, H, Flattened).

-spec update_cell(cell()) -> cell().
update_cell({dead}) -> {alive};
update_cell({alive}) -> {dead}.

move_ant({AntCellState}, #ant{pos = Pos, dir = Dir}, W, H, Occuppied) ->
  NewDir = turn(Dir, AntCellState),
  NewPos = forward(Pos, NewDir, W, H, Occuppied),
  #ant{pos = NewPos, dir = NewDir}.

forward({X, Y}, Dir, W, H, Occuppied) ->
  {DX, DY} = heading(Dir),
  NewX = torus_bounds(X + DX, W),
  NewY = torus_bounds(Y + DY, H),
  case lists:member({NewX, NewY}, Occuppied) of
    true -> {X, Y};
    false -> {NewX, NewY}
  end.

torus_bounds(Val, Max) when Val < 1 -> Max + Val;
torus_bounds(Val, Max) when Val > Max -> Val - Max;
torus_bounds(Val, _Max) -> Val.

turn(Dir, dead) -> turn_right(Dir);
turn(Dir, alive) -> turn_left(Dir).

turn_right(north) -> east;
turn_right(east) -> south;
turn_right(south) -> west;
turn_right(west) -> north.

turn_left(north) -> west;
turn_left(east) -> north;
turn_left(south) -> east;
turn_left(west) -> south.

heading(north) -> {0, 1};
heading(south) -> {0, -1};
heading(east) -> {1, 0};
heading(west) -> {-1, 0}.

create_ants(_Model, PopSize, W, H) ->
  create_ants(PopSize, W, H).

create_board(Model, W, H)->
  Model:create_board(W, H).

get_cell(Model, {X,Y}, Width, Height, Board) ->
  Model:get_cell({X,Y}, Width, Height, Board).
