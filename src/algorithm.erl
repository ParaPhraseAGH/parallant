-module(algorithm).

-export([test/1, display/3, run/3]).

-include("parallant.hrl").


%% Callbacks

-callback test() ->
    ok.

-callback display(environment(), world_impl()) ->
    ok.

-callback run(Steps, environment(), config()) -> environment()
        when Steps :: pos_integer().

%% Exported functions

-spec test(algorithm()) -> ok.
test(Alg) ->
    Alg:test().

-spec display(algorithm(), environment(), world_impl()) -> ok.
display(Alg, Env, WorldImpl) ->
    Alg:display(Env, WorldImpl).

-spec run(pos_integer(), environment(), config()) ->  environment().
run(Steps, Env, Config) ->
    Alg = Config#config.algorithm,
    Alg:run(Steps, Env, Config).
