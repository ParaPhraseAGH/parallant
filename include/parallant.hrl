%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jun 2014 12:40 PM
%%%-------------------------------------------------------------------
-author("piotr").

-type dimension() :: pos_integer().
-type position() :: {dimension(), dimension()}.
-type direction() :: north | south | east | west.
-type cell() :: {dead} | {alive}.
-type ant() :: {position(), direction()}.
-type board() :: [cell()] | gb_trees:tree().
-type model() :: list_based | gbtree_based.