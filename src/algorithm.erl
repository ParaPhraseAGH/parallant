-module(algorithm).

-export([test/1, display/2, run/3]).

-include("parallant.hrl").


% Callbacks

-callback test() ->
    ok.

-callback display(environment()) ->
    ok.

-callback run(Steps, environment(), config()) -> environment()
        when Steps :: pos_integer().

% Exported functions

-spec test(algorithm()) -> ok.
test(Alg) ->
    Alg:test().

-spec display(algorithm(), environment()) -> ok.
display(Alg, Env) ->
    Alg:display(Env).

-spec run(pos_integer(), environment(), config()) ->  environment().
run(Steps, Env, Config) ->
    Alg = Config#config.algorithm,
    Alg:run(Steps, Env, Config).
