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
    [[{_Tile, Agents}]] = algorithm:partition(Env, NColours, NParts, Config),
    Positions = algorithm:shuffle([A#agent.pos || A <- Agents]),
    NewEnv = algorithm:move_all(Positions, Env, Config),
    logger:log(NewEnv),
    log_custom(T, NewEnv, Config),
    step(T+1, MaxT, NewEnv, Config).


%% custom log every custom_log_interval iterations
-spec log_custom(Step :: pos_integer(), environment(), config()) -> ok.
log_custom(Step, Env, C) when Step rem C#config.custom_log_interval == 0 ->
    model:log_custom(Step, Env, C);
log_custom(_Step, _Env, _Config) ->
    ok.
