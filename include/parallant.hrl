%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jun 2014 12:40 PM
%%%-------------------------------------------------------------------
-author("piotr").

-record(ant, {pos :: position(),
              dir :: direction()}).

-record(world, {board :: board_state(),
                w :: dimension(),
                h :: dimension()}).

-type dimension() :: pos_integer().
-type position() :: {dimension(), dimension()}.
-type direction() :: north | south | east | west.
-type cell() :: {dead} | {alive}.
-type ant() :: #ant{}.
-type board_state() :: [cell()] | gb_trees:tree().
-type board() :: #world{}.
-type model() :: list_based | gbtree_based.