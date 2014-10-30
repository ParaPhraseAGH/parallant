%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(parallant_seq).

%% API
-export([test/0, display/1, run/2]).

-include("parallant.hrl").

-spec test() -> ok.
test() ->
    parallant:test(?MODULE).

-spec display(environment()) -> ok.
display(E) ->
    (E#env.backend):display(E#env.agents, E#env.world).

-spec run(pos_integer(), environment()) -> environment().
run(Steps, Env) ->
    step(1, Steps, Env).

-spec step(pos_integer(), pos_integer(), environment()) -> environment().
step(MaxT, MaxT, Env) ->
    Env;
step(T, MaxT, Env) ->
    Moves = parallant:get_moves(Env),
    NewEnv = parallant:apply_moves(Moves, Env),
    logger:log(NewEnv),
    step(T+1, MaxT, NewEnv).
