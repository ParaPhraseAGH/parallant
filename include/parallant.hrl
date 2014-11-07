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

-record(world, {board :: board(),
                w :: dimension(),
                h :: dimension()}).

-record(env, {
          agents :: [ant()],
          world :: world(),
          backend :: world_impl()
         }).

-record(config, {
          world_impl :: world_impl(),
          model :: 'model',
          algorithm :: algorithm()
         }).

-type dimension() :: pos_integer().
-type position() :: {dimension(), dimension()}.
-type direction() :: north | south | east | west.
-type cell() :: {dead} | {alive}.
-type ant() :: #ant{}.
-type board() :: [cell()] | gb_trees:tree().
-type world() :: #world{}.
-type algorithm() :: parallant_seq | parallant_tiled.
-type world_impl() :: list_based | gbtree_based.
-type environment() :: #env{}.
-type config() :: #config{}.
