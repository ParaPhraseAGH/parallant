%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(parallant).
%% API
-export([start/4, test/0, start/5]).
-export([update_cell/1]).

-include("parallant.hrl").
-define(MAX_WIDTH_TO_SHOW, 65).

-spec test() -> ok.
test() ->
  io:format("ListBased:~n"),
  start(list_based, 50, 30, 5, 500),
  io:format("Gb_treeBased:~n"),
  start(gbtree_based, 50, 30, 5, 500).

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

  {EndBoard, EndAnts} = step(Model, Board, Width, Height, Ants, 1, Steps),

  T2 = erlang:now(),

  if
    Width < ?MAX_WIDTH_TO_SHOW ->
      io:format("Step ~p:~n", [Steps]),
      Model:display(EndAnts, EndBoard, Width, Height);
    true -> ok
  end,

  Time = timer:now_diff(T2, T1),
  TimeInSecs = Time / 1000000,
  io:format("Time elapsed: ~p. Time per iteration: ~p s~n", [TimeInSecs, TimeInSecs / Steps]).

-spec step(model(), [cell()], dimension(), dimension(), [ant()], pos_integer(), pos_integer()) -> {[cell()],[ant()]}.
step(_Model, Board, _W, _H, Ants, MaxT, MaxT) -> {Board, Ants};
step(Model, Board, W, H, Ants, T, MaxT) ->
  AntCells = [get_cell(Model, APos, W, H, Board) || {APos, _} <- Ants],
%%   io:format("AntCells: ~p~n", [AntCells]),
  NewAnts = lists:reverse(move_ants(AntCells, Ants, W, H, [])),
  NewBoard = update_board(Model, Board, W, H, Ants),

  log(Model, NewAnts, NewBoard, T+1, W, H),

  step(Model, NewBoard, W, H, NewAnts, T + 1, MaxT).


-ifdef(debug).

log(Model, NewAnts, NewBoard, Step, Width, Height) ->
%%  lists:map(fun({_,NewADir}) -> io:format("new ant dir ~p~n",[NewADir]) end,NewAnts),
%%  lists:map(fun({NewAPos,_}) -> io:format("new ant pos ~p~n",[NewAPos]) end,NewAnts),
  io:format("Step ~p:~n", [Step + 1]),
  Model:display(NewAnts, NewBoard, Width, Height),
  timer:sleep(150),
  io:format("\033[~pA", [Height + 2]). % display in the same place as the previous step

-else.
log(_,_,_,_,_,_) ->
  ok.
-endif.

-spec create_ants(pos_integer(), dimension(), dimension()) -> [ant()].
create_ants(PopulationSize, Width, Height) ->
  ShuffledCellPositions = util:shuffle(util:all_positions(Width, Height)),
  AntPositions = lists:sublist(ShuffledCellPositions, 1, PopulationSize),
  [{Pos, util:random_direction()} || Pos <- AntPositions].

-spec move_ants([cell()], [ant()], dimension(), dimension(), position()) -> [ant()].
move_ants([], [], _, _, _) -> [];
move_ants([AntCell | TAntCells], [{AntPos, AntDir} | TAnts], W, H, Occuppied) ->
  NewAnt = move_ant(AntCell, AntPos, AntDir, W, H, Occuppied),
  {NewPos, _NewDir} = NewAnt,
  [NewAnt | move_ants(TAntCells, TAnts, W, H, [NewPos | Occuppied])].

update_board(Model, Board, W, H, Ants) ->
  Model:update_board(Board, W, H, Ants).

-spec update_cell(cell()) -> cell().
update_cell({dead}) -> {alive};
update_cell({alive}) -> {dead}.

move_ant({AntCellState}, Pos, Dir, W, H, Occuppied) ->
  NewDir = turn(Dir, AntCellState),
  NewPos = forward(Pos, NewDir, W, H, Occuppied),
  {NewPos, NewDir}.

forward({X, Y}, {DX, DY}, W, H, Occuppied) ->
  NewX = torus_bounds(X + DX, W),
  NewY = torus_bounds(Y + DY, H),
  case lists:member({NewX, NewY}, Occuppied) of
    true -> {X, Y};
    _ -> {NewX, NewY}
  end.

torus_bounds(Val, Max) when Val < 1 -> Max + Val;
torus_bounds(Val, Max) when Val > Max -> Val - Max;
torus_bounds(Val, _Max) -> Val.

turn(Dir, dead) -> turn_right(Dir);
turn(Dir, alive) -> turn_left(Dir).

turn_right({0, 1}) -> {1, 0};
turn_right({1, 0}) -> {0, -1};
turn_right({0, -1}) -> {-1, 0};
turn_right({-1, 0}) -> {0, 1}.

turn_left({0, 1}) -> {-1, 0};
turn_left({1, 0}) -> {0, 1};
turn_left({0, -1}) -> {1, 0};
turn_left({-1, 0}) -> {0, -1}.


create_ants(_Model, PopSize, W, H) ->
  create_ants(PopSize, W, H).

create_board(Model, W, H)->
  Model:create_board(W, H).

get_cell(Model, {X,Y}, Width, Height, Board) ->
  Model:get_cell({X,Y}, Width, Height, Board).
