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

-record(env, {agents :: agents:agents(),
              world :: world()}).

-record(config, {agents :: agents_impl(),
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

-type algorithm() :: algorithm_seq | algorithm_tiled.
-type model() :: model_langton | model_forams.
-type agents_impl() :: agents | agents_gbtree | agents_ets.
