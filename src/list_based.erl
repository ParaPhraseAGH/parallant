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
-export([create_board/2, update_board/4, get_cell/4, display/4]).

-spec create_board(dimension(), dimension()) -> [cell()].
create_board(Width, Height) ->
  [{dead} || _I <- lists:seq(1,Width), _J <- lists:seq(1,Height)].
%%   [{I,J} || I <- lists:seq(1,Width), J <- lists:seq(1,Height)].

-spec update_board(board(), dimension(), dimension(), [ant()]) -> board().
update_board(Board, _W, _H, []) -> Board;
update_board(Board, W, H, [#ant{pos = APos} | TAnts]) ->
  % assertion: every Ant position is different
  % TODO update board with all Ants in one pass

  Idx = pos_to_index(APos, W, H),
  NewBoard = map_nth(Idx, Board, fun parallant:update_cell/1),
  update_board(NewBoard, W, H, TAnts).

map_nth(1, [Old | Rest], F) -> [F(Old) | Rest];
map_nth(I, [E | Rest], F) -> [E | map_nth(I - 1, Rest, F)].

-spec get_cell(position(), dimension(), dimension(), [cell()]) -> cell().
get_cell({X, Y}, Width, _Height, Board) ->
  Idx = pos_to_index({X, Y}, Width, _Height),
%%   io:format("X:~p, Y:~p, W:~p, I:~p~n",[X,Y,Width,Idx]),
  lists:nth(Idx, Board).

pos_to_index({X, Y}, _W, H) ->
  (X - 1) * H + Y.

-spec display(ant(), board(), dimension(), dimension()) -> ok.
display(Ants, Board, Width, Height) ->
  graphics:display(Ants, Board, Width, Height).