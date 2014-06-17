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
-export([create_board/2, update_board/4, get_cell/4, display/4]).

-spec create_board(dimension(), dimension()) -> [gb_trees:tree(position(), cell())].
create_board(Width, Height) ->
  Tree = gb_trees:empty(),
  Indices = util:all_positions(Width, Height),
  populateTree({dead}, Indices, Tree).

-spec populateTree(cell(), [position()], gb_trees:tree()) -> gb_trees:tree().
populateTree(_, [], Tree) -> Tree;
populateTree(Val, [HI | TI], Tree) ->
  populateTree(Val, TI, gb_trees:insert(HI, Val, Tree)).

-spec update_board(gb_trees:tree(position(), cell()), dimension(), dimension(), [ant()]) -> gb_trees:tree(position(), cell()).
update_board(Board, _W, _H, []) -> Board;
update_board(Board, W, H, [{APos, _ADir} | TAnts]) ->
  % assertion: every Ant position is different
  ACell = gb_trees:get(APos, Board),
  NewBoard = gb_trees:update(APos, parallant:update_cell(ACell), Board),
  update_board(NewBoard, W, H, TAnts).

-spec get_cell(position(), dimension(), dimension(), gb_trees:tree(position(), cell())) -> cell().
get_cell(Pos, _Width, _Height, Board) ->
  gb_trees:get(Pos, Board).

-spec display([ant()], board(), dimension(), dimension()) -> ok.
display(Ants, Board, Width, Height) ->
  %%   io:format("RawBoard: ~p~n",[Board]),
    BoardList = gb_trees:values(Board),
%%   BoardList = gb_trees:to_list(Board),
  graphics:display(Ants, BoardList, Width, Height).