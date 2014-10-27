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

-include("parallant.hrl").

%% API
-export([create_board/2, update_board/2, get_cell/2, display/2, update_cell/2]).

-spec create_board(dimension(), dimension()) -> [gb_trees:tree(position(), cell())].
create_board(Width, Height) ->
    Tree = gb_trees:empty(),
    Indices = util:all_positions(Width, Height),
    populateTree({dead}, Indices, Tree).

-spec populateTree(cell(), [position()], gb_trees:tree()) -> gb_trees:tree().
populateTree(_, [], Tree) -> Tree;
populateTree(Val, [HI | TI], Tree) ->
    populateTree(Val, TI, gb_trees:insert(HI, Val, Tree)).

-spec update_board(world(), [ant()]) -> world().
update_board(World, []) -> World;
update_board(W, [#ant{pos = Pos} | TAnts]) ->
    % assertion: every Ant position is different
    NewWorld = update_cell(Pos, W),
    update_board(NewWorld, TAnts).

-spec update_cell(position(), world()) -> world().
update_cell(Pos, W) ->
    Cell = get_cell(Pos, W),
    NewBoard = gb_trees:update(Pos, parallant:update_cell(Cell), W#world.board),
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
