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

-record(tile, {min_x :: dimension(),
               max_x :: dimension(),
               min_y :: dimension(),
               max_y :: dimension()}).

-record(env, {
          agents :: [ant()],
          world :: board(),
          backend :: world_impl()
         }).

-type dimension() :: pos_integer().
-type position() :: {dimension(), dimension()}.
-type direction() :: north | south | east | west.
-type cell() :: {dead} | {alive}.
-type tile() :: #tile{}.
-type ant() :: #ant{}.
-type board_state() :: [cell()] | gb_trees:tree().
-type board() :: #world{}.
-type model() :: parallant_seq | parallant_tiled.
-type world_impl() :: list_based | gbtree_based.
-type environment() :: #env{}.
