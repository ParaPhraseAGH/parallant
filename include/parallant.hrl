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
              state :: parallant:ant_state()}).

-record(world, {w :: dimension(),
                h :: dimension()}).

-record(env, {agents :: ants_impl:ants(),
              world :: world()}).

-record(config, {ants_impl :: ants_impl(),
                 model :: model(),
                 algorithm  :: algorithm(),
                 log :: boolean(),
                 animate :: boolean()}).

-type ant() :: #ant{}.
-type world() :: #world{}.
-type environment() :: #env{}.
-type config() :: #config{}.

-type dimension() :: pos_integer().
-type position() :: {dimension(), dimension()}.

-type algorithm() :: parallant_seq | parallant_tiled.
-type model() :: model_langton | model_forams.
-type ants_impl() :: ants | ants_gbt | ants_ets.
