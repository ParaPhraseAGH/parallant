%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(parallant).
%% -compile(export_all).
%% API
-export([start/3, start/0]).

%% -define(DEBUG,true).

-type dimension() :: pos_integer().
-type onedir() :: -1 | 0 | 1.
-type position() :: {dimension(), dimension()}.
-type direction() :: {onedir(),onedir()}.
-type cell() :: {dead} | {alive}.
-type ant() :: {position(), direction()}.

-spec start() -> ok.
start() ->
  start(65,45,10000).

-spec start(dimension(), dimension(), pos_integer()) -> ok.
start(Width, Height, Steps) ->
  Board = create_board(Width, Height),
  Ant = create_ant(Width, Height),
  io:format("Step 1:~n"),
  display(Ant, Board, Width, Height),
  T1 = erlang:now(),
  {EndBoard, EndAnt} = step(Board, Width, Height, Ant, 1, Steps),
  T2 = erlang:now(),
  io:format("Step ~p:~n",[Steps]),
  display(EndAnt, EndBoard, Width, Height),
  Time = timer:now_diff(T2,T1),
  io:format("Time elapsed: ~p s~n",[Time/1000000]).


step(Board, _W, _H, Ant, MaxT, MaxT) -> {Board, Ant};
step(Board, W, H, {APos, ADir}, T, MaxT) ->
  AntCell = get_cell(APos, W, H, Board),
  {NewAPos, NewADir} = move_ant(AntCell, APos, ADir, W, H),
  NewBoard = update_board(Board, W, H, APos, NewAPos),

%%   io:format("new ant dir ~p~n",[NewADir]),
%%   io:format("Step ~p:~n",[T+1]),
%%   io:format("new ant pos ~p~n",[NewAPos]),
%%   display({NewAPos, NewADir}, NewBoard, W, H),

  step(NewBoard, W, H, {NewAPos, NewADir}, T+1, MaxT).

update_board(Board, W, H, APos, _NewAPos) ->
  Idx = ant_pos_index(APos, W, H),
  map_nth(Idx,Board,
    fun
      ({dead}) -> {alive};
      ({alive})-> {dead}
    end).

map_nth(1, [Old|Rest], F) -> [F(Old)|Rest];
map_nth(I, [E|Rest], F) -> [E|map_nth(I-1, Rest, F)].

move_ant({AntCellState}, Pos, Dir, W, H) ->
  NewDir = turn(Dir, AntCellState),
  NewPos = forward(Pos, NewDir, W, H),
  {NewPos, NewDir}.

forward({X, Y}, {DX, DY}, W, H) ->
  {
    bounds(X + DX, W),
    bounds(Y + DY, H)
  }.

bounds(Val, Max) when Val < 1 -> Max + Val;
bounds(Val, Max) when Val > Max -> Val - Max;
bounds(Val, _Max) -> Val.

turn(Dir, dead)  -> turn_right(Dir);
turn(Dir, alive) -> turn_left(Dir).

turn_right({0,  1}) -> {1,  0};
turn_right({1,  0}) -> {0, -1};
turn_right({0, -1}) -> {-1, 0};
turn_right({-1, 0}) -> {0,  1}.

turn_left({0,  1}) -> {-1, 0};
turn_left({1,  0}) -> {0,  1};
turn_left({0, -1}) -> {1,  0};
turn_left({-1, 0}) -> {0, -1}.


-spec create_ant(dimension(), dimension()) -> ant().
create_ant(Width, Height) ->
  {{(1+Width) div 2, (1+Height) div 2}, {0, -1}}.

-spec create_board(dimension(), dimension()) -> [cell()].
create_board(Width, Height) ->
  [{dead} || _I <- lists:seq(1,Width*Height)].
%%   [{I} || I <- lists:seq(1,Width*Height)].


%% -spec get_cell(dimension(), [cell()]) -> cell().
%% get_cell(I, Board) ->
%%   lists:nth(I, Board).

-spec get_cell(position(), dimension(), dimension(), [cell()]) -> cell().
get_cell({X, Y}, Width, _Height, Board) ->
  lists:nth((Y-1)*Width+X, Board).

-spec cell_char(cell()) -> char().
cell_char({alive}) -> $o;
cell_char({dead}) -> $.;
cell_char({I}) -> I.

-spec ant_char(position()) -> char().
ant_char({-1, 0}) -> $<;
ant_char({1 , 0}) -> $>;
ant_char({0 , 1}) -> $^;
ant_char({0 ,-1}) -> $v.

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

flip_ant({{X,Y}, _Dir}, H) -> {{X, H-Y+1}, _Dir}.

display_cell(I, Cell, W, H, {{X,Y}, ADir}) ->
  AI = ant_pos_index({X,Y}, W, H),
  C = if
    I == AI -> ant_char(ADir);
    true -> cell_char(Cell)
  end,
  io:format("~c ",[C]).
%%   io:format("~p ",[C]).

ant_pos_index({X, Y}, W, _H) ->
  (Y - 1)*W+X.

flip_board(Board, Width) ->
  flip_board_acc(Board, [], [], Width, 1).

flip_board_acc([], _Row, Rows, _, _) ->
    lists:flatten(Rows);
flip_board_acc([H | T], Row, Rows, Width, Width) ->
  flip_board_acc(T, [], [lists:reverse([H|Row])| Rows], Width, 1);
flip_board_acc([H | T], Row, Rows, Width, RI) ->
  flip_board_acc(T, [H | Row], Rows, Width, RI+1).

display(Ant, Board, W, H) ->
  display(flip_ant(Ant, H), flip_board(Board, W), W, H, 1).
%%   display(Ant, Board, W, H, 1).

display(_Ant, [], _W, _H, _N) ->
  io:format("~n");
display(Ant, [HB|TB], W, H, I) when I rem W == 0 ->
  display_cell(I, HB, W, H, Ant),
  io:format("~n"),
  display(Ant, TB, W, H, I+1);
display(Ant, [HB|TB], W, H, I) ->
  display_cell(I, HB, W, H, Ant),
  display(Ant, TB, W, H, I+1).

