%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jun 2014 12:29 PM
%%%-------------------------------------------------------------------
-module(gbtree_based).
-author("piotr").

-behaviour(world_impl).
-include("parallant.hrl").

%% API
-export([create_board/2, update_board/3, get_cell/2, display/2, update_cell/3]).

-spec create_board(dimension(), dimension()) ->
                          [gb_trees:tree(position(), cell())].
create_board(Width, Height) ->
    Tree = gb_trees:empty(),
    Indices = [{I,J} || I <- lists:seq(1, Width), J <-lists:seq(1,Height)],
    populateTree({dead}, Indices, Tree).

-spec populateTree(cell(), [position()], gb_trees:tree()) -> gb_trees:tree().
populateTree(_, [], Tree) -> Tree;
populateTree(Val, [HI | TI], Tree) ->
    populateTree(Val, TI, gb_trees:insert(HI, Val, Tree)).

-spec update_board(world(), [ant()], config()) -> world().
update_board(World, [], _Config) -> World;
update_board(W, [#ant{pos = Pos} | TAnts], Config) ->
    % assertion: every Ant position is different
    NewWorld = update_cell(Pos, W, Config),
    update_board(NewWorld, TAnts, Config).

-spec update_cell(position(), world(), config()) -> world().
update_cell(Pos, W, Config) ->
    Cell = get_cell(Pos, W),
    Model = Config#config.model,
    NewBoard = gb_trees:update(Pos,
                               model:update_cell(Model, Cell),
                               W#world.board),
    W#world{board = NewBoard}.

-spec get_cell(position(), world()) -> cell().
get_cell(Pos, W) ->
    gb_trees:get(Pos, W#world.board).

-spec display([ant()], world()) -> ok.
display(Ants, W) ->
    %%   io:format("RawBoard: ~p~n",[Board]),
    BoardList = gb_trees:values(W#world.board),
    %%   BoardList = gb_trees:to_list(Board),
    graphics:display(Ants, W#world{board = BoardList}).
