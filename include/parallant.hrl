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
-type onedir() :: -1 | 0 | 1.
-type position() :: {dimension(), dimension()}.
-type direction() :: {onedir(), onedir()}.
-type cell() :: {dead} | {alive}.
-type ant() :: {position(), direction()}.
-type board() :: [cell()].
-type tree() :: term().
-type model() :: list_based | gbtree_based.