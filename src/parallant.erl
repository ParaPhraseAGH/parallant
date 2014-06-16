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
-export([start/3, start/0, start/4]).
-export([ant_char/1, cell_char/1, update_cell/1,
  all_positions/2, shuffle/1, random_direction/0]).

%% -define(debug, ok).
-define(IMPL, list_based).
%% -define(IMPL, gbtree_based).

-include("parallant.hrl").

-spec start() -> ok.
start() ->
  start(50, 48, 1, 500).

-spec start(dimension(), dimension(), pos_integer()) -> ok.
start(Width, Height, Steps) ->
  start(Width, Height, 1, Steps).

-spec start(dimension(), dimension(), pos_integer(), pos_integer()) -> ok.
start(Width, Height, PopulationSize, Steps) ->
  Board = create_board(Width, Height),
  Ants = create_ants(PopulationSize, Width, Height),

%%   io:format("Ants: ~p~n", [Ants]),
  io:format("Step 1:~n"),
  ?IMPL:display(Ants, Board, Width, Height),
  T1 = erlang:now(),

  {EndBoard, EndAnts} = step(Board, Width, Height, Ants, 1, Steps),

  T2 = erlang:now(),

  io:format("Step ~p:~n", [Steps]),
  ?IMPL:display(EndAnts, EndBoard, Width, Height),
  Time = timer:now_diff(T2, T1),
  TimeInSecs = Time / 1000000,
  io:format("Time elapsed: ~p. Time per iteration: ~p s~n", [TimeInSecs, TimeInSecs / Steps]).

-spec step([cell()], dimension(), dimension(), [ant()], pos_integer(), pos_integer()) -> {[cell()],[ant()]}.
step(Board, _W, _H, Ants, MaxT, MaxT) -> {Board, Ants};
step(Board, W, H, Ants, T, MaxT) ->
  AntCells = [get_cell(APos, W, H, Board) || {APos, _} <- Ants],
  NewAnts = lists:reverse(move_ants(AntCells, Ants, W, H, [])),
  NewBoard = update_board(Board, W, H, Ants),

  log(NewAnts, NewBoard, T+1, W, H),

  step(NewBoard, W, H, NewAnts, T + 1, MaxT).


-ifdef(debug).

log(NewAnts, NewBoard, Step, Width, Height) ->
    lists:map(fun({_,NewADir}) -> io:format("new ant dir ~p~n",[NewADir]) end,NewAnts),
    lists:map(fun({NewAPos,_}) -> io:format("new ant pos ~p~n",[NewAPos]) end,NewAnts),
  io:format("Step ~p:~n", [Step + 1]),
  ?IMPL:display(NewAnts, NewBoard, Width, Height),
  timer:sleep(500). %
%%   ,io:format("\033[~pA", [Height + 2]). % display in the same place as the previous step

-else.
log(_,_,_,_,_) ->
  ok.
-endif.

-spec move_ants([cell()], [ant()], dimension(), dimension(), position()) -> [ant()].
move_ants([], [], _, _, _) -> [];
move_ants([AntCell | TAntCells], [{AntPos, AntDir} | TAnts], W, H, Occuppied) ->
  NewAnt = move_ant(AntCell, AntPos, AntDir, W, H, Occuppied),
  {NewPos, _NewDir} = NewAnt,
  [NewAnt | move_ants(TAntCells, TAnts, W, H, [NewPos | Occuppied])].

update_board(Board, W, H, Ants) ->
  ?IMPL:update_board(Board, W, H, Ants).

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


create_ants(PopSize, W, H) ->
  ?IMPL:create_ants(PopSize, W, H).

create_board(W, H)->
  ?IMPL:create_board(W, H).


get_cell({X,Y}, Width, Height, Board) ->
  ?IMPL:get_cell({X,Y}, Width, Height, Board).

-spec cell_char(cell()) -> char().
cell_char({alive}) -> $o;
cell_char({dead}) -> $.;
cell_char({I}) -> I;
cell_char(_V) -> _V.

-spec ant_char(position()) -> char().
ant_char({-1, 0}) -> $<;
ant_char({1, 0}) -> $>;
ant_char({0, 1}) -> $^;
ant_char({0, -1}) -> $v.

%% display(_, W, H, N) when N > W * H -> ok;
%% display(Board, W, H, I) when I rem W == 0 ->
%%   X = (I-1) div W + 1,
%%   Y = I rem W if I%W >0 else W,
%%   io:format("~p~n",[cell_char(get_cell(X, Y, W, H, Board))]),
%%   display(Board, W, H, I+1);
%% display(Board, _W, _H, I) ->
%%   X = (I-1) div W + 1,
%%   Y = I rem W if I%W >0 else W,
%%   io:format("~p",[cell_char(get_cell(X, Y, W, H,Board))]),
%%   display(Board, _W, _H, I+1).

-spec all_positions(dimension(), dimension()) -> [{dimension(),dimension()}].
all_positions(Width, Height) ->
  [{I,J} || I <- lists:seq(1, Width), J <-lists:seq(1,Height)].
-spec shuffle(list()) -> list().
shuffle(L) ->
  [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].


-spec random_direction() -> direction().
random_direction() ->
  Dirs = [{0, 1}, {1, 0}, {0, -1}, {-1, 0}],
  Idx = random:uniform(length(Dirs)),
  lists:nth(Idx, Dirs).