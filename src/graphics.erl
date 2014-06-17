%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jun 2014 2:33 PM
%%%-------------------------------------------------------------------
-module(graphics).
-author("piotr").

-include("parallant.hrl").
%% API
-export([display/4]).


%% ant_index_to_pos(I, W, _H) when I rem W > 0 ->
%%   {I rem W, (I - 1) div W + 1};
%% ant_index_to_pos(I, W, _H) ->
%%   {W, (I - 1) div W + 1}.

ant_pos_to_index({X, Y}, W, _H) ->
  I = (Y - 1) * W + X,
%%   io:format("X:~p, Y:~p, W:~p, I:~p~n",[X,Y,W,I]),
  I.

flip_ant({{X, Y}, _Dir}, H) -> {{X, H - Y + 1}, _Dir}.

flip_board(Board, Width) ->
  flip_board_acc(Board, [], [], Width, 1).

flip_board_acc([], _Row, Rows, _, _) ->
%%   lists:flatten(Rows);
  Rows;
flip_board_acc([H | T], Row, Rows, Width, Width) ->
  flip_board_acc(T, [], [lists:reverse([H | Row]) | Rows], Width, 1);
flip_board_acc([H | T], Row, Rows, Width, RI) ->
  flip_board_acc(T, [H | Row], Rows, Width, RI + 1).

display_cell(I, Cell, _W, _H, AntsWithIndex) ->
  Result = lists:keytake(I, 1, AntsWithIndex),
  {C, RestOfAnts} = case Result of
                      {value, {_I, {_APos, ADir}}, Rest} ->
                        {parallant:ant_char(ADir), Rest};
                      false ->
                        {parallant:cell_char(Cell), AntsWithIndex}
                    end,
  io:format("~c ", [C]),
  RestOfAnts.

-spec display([ant()], board(), dimension(), dimension()) -> ok.
display(Ants, Board, W, H) ->
%%   io:format("RawBoard: ~p~n",[Board]),
  FlippedAnts = [flip_ant(Ant, H) || Ant <- Ants],
  FlippedAntsWithIndex = [{ant_pos_to_index(APos, W, H), Ant} || Ant = {APos, _Dir} <- FlippedAnts],
  FlippedBoard = lists:reverse(flip_board(Board, H)),
%%   io:format("FlippedBoard: ~p~n",[FlippedBoard]),
  TransposedBoard = lists:reverse(transpose_board(FlippedBoard)),
%%   io:format("TransposedBoard: ~p~n",[TransposedBoard]),
  display(lists:keysort(1, FlippedAntsWithIndex), lists:flatten(TransposedBoard), W, H, 1).

transpose_board([[]|_]) -> [];
transpose_board(M) ->
  [lists:map(fun hd/1, M) | transpose_board(lists:map(fun tl/1, M))].

display(_Ants, [], _W, _H, _N) ->
  io:format("~n");
display(Ants, [HB | TB], W, H, I) when I rem W == 0 ->
  RestOfAnts = display_cell(I, HB, W, H, Ants),
  io:format("~n"),
  display(RestOfAnts, TB, W, H, I + 1);
display(Ants, [HB | TB], W, H, I) ->
  RestOfAnts = display_cell(I, HB, W, H, Ants),
  display(RestOfAnts, TB, W, H, I + 1).