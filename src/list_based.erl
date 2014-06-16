%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jun 2014 12:41 PM
%%%-------------------------------------------------------------------
-module(list_based).
-author("piotr").

-include("parallant.hrl").
%% API
%% -export([]).
-compile(export_all).

-spec create_board(dimension(), dimension()) -> [cell()].
create_board(Width, Height) ->
  [{dead} || _I <- lists:seq(1,Width), _J <- lists:seq(1,Height)].
%%   [{I,J} || I <- lists:seq(1,Width), J <- lists:seq(1,Height)].

-spec update_board(board(), dimension(), dimension(), [ant()]) -> board().
update_board(Board, _W, _H, []) -> Board;
update_board(Board, W, H, [{APos, _ADir} | TAnts]) ->
  % assertion: every Ant position is different
  % TODO update board with all Ants in one pass

  % FIXME updating ant positions

  Idx = ant_pos_to_index(APos, W, H),
  NewBoard = map_nth(Idx, Board,
    fun parallant:update_cell/1),
  update_board(NewBoard, W, H, TAnts).

map_nth(1, [Old | Rest], F) -> [F(Old) | Rest];
map_nth(I, [E | Rest], F) -> [E | map_nth(I - 1, Rest, F)].

-spec get_cell(position(), dimension(), dimension(), [cell()]) -> cell().
get_cell({X, Y}, Width, _Height, Board) ->
%%   Idx = (Y - 1) * Width + X,
  Idx = ant_pos_to_index({X, Y}, Width, _Height),
%%   io:format("X:~p, Y:~p, W:~p, I:~p~n",[X,Y,Width,Idx]),
  lists:nth(Idx, Board).

ant_pos_to_index({X, Y}, W, _H) ->
  (Y - 1) * W + X.

-spec create_ants(pos_integer(), dimension(), dimension()) -> [ant()].
create_ants(PopulationSize, Width, Height) ->
  ShuffledCellPositions = parallant:shuffle(parallant:all_positions(Width, Height)),
  AntPositions = lists:sublist(ShuffledCellPositions, 1, PopulationSize),
  [{Pos, parallant:random_direction()} || Pos <- AntPositions].

-spec shuffle(list()) -> list().
shuffle(L) ->
  [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].


-spec random_direction() -> direction().
random_direction() ->
  Dirs = [{0, 1}, {1, 0}, {0, -1}, {-1, 0}],
  Idx = random:uniform(length(Dirs)),
  lists:nth(Idx, Dirs).

display(Ants, Board, Width, Height) ->
  graphics:display(Ants, Board, Width, Height).