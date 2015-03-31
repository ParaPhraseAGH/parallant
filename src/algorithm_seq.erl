%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(algorithm_seq).
-behaviour(algorithm).
%% API
-export([run/3]).

-include("parallant.hrl").


-spec run(Steps :: pos_integer(), environment(), config()) -> environment().
run(Steps, Env, Config) ->
    step(1, Steps, Env, Config).

-spec step(Iteration :: pos_integer(), MaxIteration :: pos_integer(),
           environment(), config()) -> environment().
step(Iteration, MaxIteration, Env, _Config) when Iteration =:= MaxIteration ->
    Env;
step(Iteration, MaxIteration, Env, Config) ->
    Agents = agents:get_list(Env#env.agents, Config),

    Positions = algorithm:shuffle([A#agent.pos || A <- Agents]),
    NewEnv = algorithm:move_all(Positions, Env, Config),
    logger:log(NewEnv),
    algorithm:log_custom(Iteration, NewEnv, Config),
    step(Iteration+1, MaxIteration, NewEnv, Config).
