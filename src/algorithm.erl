-module(algorithm).

-export([test/1, run/3]).

-include("parallant.hrl").


%% Callbacks

-callback test() ->
    ok.

-callback run(Steps, environment(), config()) -> environment()
                                                     when Steps :: pos_integer().

%% Exported functions

-spec test(algorithm()) -> ok.
test(Alg) ->
    Alg:test().

-spec run(Steps :: pos_integer(), environment(), config()) ->  environment().
run(Steps, Env, Config) ->
    Alg = Config#config.algorithm,
    Alg:run(Steps, Env, Config).
