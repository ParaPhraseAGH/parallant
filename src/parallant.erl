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
-export([start/3, start/0, start/4]).

%% -define(debug, ok).

-type dimension() :: pos_integer().
-type onedir() :: -1 | 0 | 1.
-type position() :: {dimension(), dimension()}.
-type direction() :: {onedir(), onedir()}.
-type cell() :: {dead} | {alive}.
-type ant() :: {position(), direction()}.

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
  display(Ants, Board, Width, Height),
  T1 = erlang:now(),

  {EndBoard, EndAnts} = step(Board, Width, Height, Ants, 1, Steps),

  T2 = erlang:now(),

  io:format("Step ~p:~n", [Steps]),
  display(EndAnts, EndBoard, Width, Height),
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
  %%   lists:map(fun({_,NewADir}) -> io:format("new ant dir ~p~n",[NewADir]) end,NewAnts),
  %%   lists:map(fun({NewAPos,_}) -> io:format("new ant pos ~p~n",[NewAPos]) end,NewAnts),
  io:format("Step ~p:~n", [Step + 1]),
  display(NewAnts, NewBoard, Width, Height),
  timer:sleep(50),
  io:format("\033[~pA", [Height + 2]).

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

update_board(Board, _W, _H, []) -> Board;
update_board(Board, W, H, [{APos, _ADir} | TAnts]) ->
  % assertion: every Ant position is different
  % TODO update board with all Ants in one pass
  Idx = ant_pos_to_index(APos, W, H),
  NewBoard = map_nth(Idx, Board,
    fun
      ({dead}) -> {alive};
      ({alive}) -> {dead}
    end),
  update_board(NewBoard, W, H, TAnts).

map_nth(1, [Old | Rest], F) -> [F(Old) | Rest];
map_nth(I, [E | Rest], F) -> [E | map_nth(I - 1, Rest, F)].

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

-spec create_ants(pos_integer(), dimension(), dimension()) -> [ant()].
create_ants(PopulationSize, Width, Height) ->
  ToAntPos = fun(I) -> {ant_index_to_pos(I, Width, Height), random_direction()} end,
  ShuffledCellIndices = shuffle(lists:seq(1, Width * Height)),
  AntIndices = lists:sublist(ShuffledCellIndices,1,PopulationSize),
  lists:map(ToAntPos, AntIndices).

-spec shuffle(list()) -> list().
shuffle(L) ->
  [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].

-spec random_direction() -> direction().
random_direction() ->
  Dirs = [{0, 1}, {1, 0}, {0, -1}, {-1, 0}],
  Idx = random:uniform(length(Dirs)),
  lists:nth(Idx, Dirs).


-spec create_board(dimension(), dimension()) -> [cell()].
create_board(Width, Height) ->
  [{dead} || _I <- lists:seq(1, Width * Height)].
%%   [{I} || I <- lists:seq(1,Width*Height)].


-spec get_cell(position(), dimension(), dimension(), [cell()]) -> cell().
get_cell({X, Y}, Width, _Height, Board) ->
%%   Idx = (Y - 1) * Width + X,
  Idx = ant_pos_to_index({X, Y}, Width, _Height),
%%   io:format("X:~p, Y:~p, W:~p, I:~p~n",[X,Y,Width,Idx]),
  lists:nth(Idx, Board).

-spec cell_char(cell()) -> char().
cell_char({alive}) -> $o;
cell_char({dead}) -> $.;
cell_char({I}) -> I.

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


display_cell(I, Cell, _W, _H, AntsWithIndex) ->
  Result = lists:keytake(I, 1, AntsWithIndex),
  {C, RestOfAnts} = case Result of
                      {value, {_I, {_APos, ADir}}, Rest} -> {ant_char(ADir), Rest};
                      false ->
                        {cell_char(Cell), AntsWithIndex}
                    end,
  io:format("~c ", [C]),
  RestOfAnts.


ant_pos_to_index({X, Y}, W, _H) ->
  (Y - 1) * W + X.

ant_index_to_pos(I, W, _H) when I rem W > 0 ->
  {I rem W, (I - 1) div W + 1};
ant_index_to_pos(I, W, _H) ->
  {W, (I - 1) div W + 1}.

flip_ant({{X, Y}, _Dir}, H) -> {{X, H - Y + 1}, _Dir}.

flip_board(Board, Width) ->
  flip_board_acc(Board, [], [], Width, 1).

flip_board_acc([], _Row, Rows, _, _) ->
  lists:flatten(Rows);
flip_board_acc([H | T], Row, Rows, Width, Width) ->
  flip_board_acc(T, [], [lists:reverse([H | Row]) | Rows], Width, 1);
flip_board_acc([H | T], Row, Rows, Width, RI) ->
  flip_board_acc(T, [H | Row], Rows, Width, RI + 1).

display(Ants, Board, W, H) ->
  FlippedAnts = [flip_ant(Ant, H) || Ant <- Ants],
  FlippedAntsWithIndex = [{ant_pos_to_index({X, Y}, W, H), Ant} || Ant = {{X, Y}, _Dir} <- FlippedAnts],
  display(lists:keysort(1, FlippedAntsWithIndex), flip_board(Board, W), W, H, 1).
%%   display(Ant, Board, W, H, 1).

display(_Ants, [], _W, _H, _N) ->
  io:format("~n");
display(Ants, [HB | TB], W, H, I) when I rem W == 0 ->
  RestOfAnts = display_cell(I, HB, W, H, Ants),
  io:format("~n"),
  display(RestOfAnts, TB, W, H, I + 1);
display(Ants, [HB | TB], W, H, I) ->
  RestOfAnts = display_cell(I, HB, W, H, Ants),
  display(RestOfAnts, TB, W, H, I + 1).

