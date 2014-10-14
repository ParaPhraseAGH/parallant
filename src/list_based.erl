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
-export([create_board/2, update_board/2, get_cell/2, display/2]).

-spec create_board(dimension(), dimension()) -> [cell()].
create_board(Width, Height) ->
    [{dead} || _I <- lists:seq(1,Width), _J <- lists:seq(1,Height)].
%%   [{I,J} || I <- lists:seq(1,Width), J <- lists:seq(1,Height)].

-spec update_board(board(), [ant()]) -> board().
update_board(World, []) -> World;
update_board(W, [#ant{pos = APos} | TAnts]) ->
    % assertion: every Ant position is different
    % TODO update board with all Ants in one pass
    Idx = pos_to_index(APos, W#world.w, W#world.h),
    NewBoard = map_nth(Idx, W#world.board, fun parallant:update_cell/1),
    update_board(W#world{board = NewBoard}, TAnts).


map_nth(1, [Old | Rest], F) -> [F(Old) | Rest];
map_nth(I, [E | Rest], F) -> [E | map_nth(I - 1, Rest, F)].

-spec get_cell(position(), board()) -> cell().
get_cell({X, Y}, #world{board = Board, w = W, h = H}) ->
    Idx = pos_to_index({X, Y}, W, H),
    %%   io:format("X:~p, Y:~p, W:~p, I:~p~n",[X,Y,Width,Idx]),
    lists:nth(Idx, Board).

pos_to_index({X, Y}, _W, H) ->
    (X - 1) * H + Y.

-spec display([ant()], board()) -> ok.
display(Ants, World) ->
    graphics:display(Ants, World).
