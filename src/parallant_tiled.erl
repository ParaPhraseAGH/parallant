%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(parallant_tiled).

-behaviour(algorithm).
%% API
-export([test/0, display/1, run/3]).

-include("parallant.hrl").

-spec test() -> ok.
test() ->
    parallant:test(?MODULE).

%% TODO
-spec display(environment()) -> ok.
display(E = #env{agents = Ants}) when is_list(Ants) ->
    (E#env.backend):display(Ants, E#env.world); %% List case
display(E) ->
  (E#env.backend):display(gb_trees:values(E#env.agents), E#env.world). %% gb_trees case

-spec run(pos_integer(), environment(), model()) -> environment().
run(Steps, Env, Model) ->
    step(1, Steps, Env, Model).

-spec step(pos_integer(), pos_integer(), environment(), model()) -> environment().
step(MaxT, MaxT, Env, _Model) ->
    Env;
step(T, MaxT, Env, Model) ->
    NColours = 2,
    NParts = 2,
    Partitioned = Model:partition(Env, NColours, NParts),

    ProcessTile =
        fun(Tile, E) ->
                F = fun (A) -> parallant:get_moves(E#env{agents = A}) end,
                %% Moves = F(Tile),
                Moves = lists:flatmap(F, Tile),

                parallant:apply_moves(Moves, E, Model)
        end,
    NewEnv = lists:foldl(ProcessTile, Env, Partitioned),
    logger:log(NewEnv),
    step(T+1, MaxT, NewEnv, Model).
