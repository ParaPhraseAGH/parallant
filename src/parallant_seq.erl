%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(parallant_seq).
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
    NColours = 1,
    NParts = 1,
    [AntList] = Model:partition(Env, NColours, NParts),

    Moves = parallant:get_moves(Env#env{agents = AntList}),
    NewEnv = parallant:apply_moves(Moves, Env, Model),
    logger:log(NewEnv),
    step(T+1, MaxT, NewEnv, Model).
