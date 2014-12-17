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
-export([test/0, run/3]).

-include("parallant.hrl").

-spec test() -> ok.
test() ->
    parallant:test(?MODULE).

-spec run(Steps :: pos_integer(), environment(), config()) -> environment().
run(Steps, Env, Config) ->
    step(1, Steps, Env, Config).

-spec step(T :: pos_integer(), MaxT :: pos_integer(),
           environment(), config()) -> environment().
step(MaxT, MaxT, Env, _Config) ->
    Env;
step(T, MaxT, Env, Config) ->
    NColours = 1,
    NParts = 1,
    [Ants] = ants_impl:partition(Env, NColours, NParts, Config),

    %% Moves = parallant:get_moves(Env#env{agents = AntList}, Config),
    %% NewEnv = parallant:apply_moves(Moves, Env, Config),
    NewEnv = parallant:move_all(Env#env{agents = shuffle(Ants)}, Config),
    logger:log(NewEnv),
    step(T+1, MaxT, NewEnv, Config).

-spec shuffle(list()) -> list().
shuffle(L) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].
