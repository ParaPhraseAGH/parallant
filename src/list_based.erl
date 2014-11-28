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

-behaviour(world_impl).
-include("parallant.hrl").

%% API
-export([create_board/3, update_board/3, get_cell/2, display/2, update_cell/3]).

-spec create_board(dimension(), dimension(), config()) -> [cell()].
create_board(Width, Height, Config) ->
    [model:initial_cell_state(Config) || _I <- lists:seq(1, Width),
                                         _J <- lists:seq(1, Height)].
%%   [{I,J} || I <- lists:seq(1,Width), J <- lists:seq(1,Height)].

-spec update_board(world(), [ant()], config()) -> world().
update_board(World, [], _Config) -> World;
update_board(W, [#ant{pos = APos} | TAnts], Config) ->
    %% assertion: every Ant position is different
    %% TODO update board with all Ants in one pass
    NewWorld = update_cell(APos, W, Config),
    update_board(NewWorld, TAnts, Config).

-spec update_cell(position(), world(), config()) -> world().
update_cell(Pos, W, Config) ->
    Idx = pos_to_index(Pos, W#world.w, W#world.h),
    UpdateCell = fun(CellState) -> model:update_cell(CellState, Config) end,
    NewBoard = map_nth(Idx, W#world.board, UpdateCell),
    W#world{board = NewBoard}.


map_nth(1, [Old | Rest], F) -> [F(Old) | Rest];
map_nth(I, [E | Rest], F) -> [E | map_nth(I - 1, Rest, F)].

-spec get_cell(position(), world()) -> cell().
get_cell({X, Y}, #world{board = Board, w = W, h = H}) ->
    Idx = pos_to_index({X, Y}, W, H),
    %%   io:format("X:~p, Y:~p, W:~p, I:~p~n",[X,Y,Width,Idx]),
    lists:nth(Idx, Board).

pos_to_index({X, Y}, _W, H) ->
    (X - 1) * H + Y.

-spec display([ant()], world()) -> ok.
display(Ants, World) ->
    graphics:display(Ants, World).
