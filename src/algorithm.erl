-module(algorithm).

-export([test/1, run/3, shuffle/1, move_all/3]).

-include("parallant.hrl").


%% Callbacks

-callback test() ->
    ok.

-callback run(Steps, environment(), config()) -> environment() when
      Steps :: pos_integer().

%% Exported functions

-spec test(algorithm()) -> ok.
test(Alg) ->
    Alg:test().

-spec run(Steps :: pos_integer(), environment(), config()) ->  environment().
run(Steps, Env, Config) ->
    Alg = Config#config.algorithm,
    Alg:run(Steps, Env, Config).

-spec move_all([position()], environment(), config()) -> environment().
move_all(Positions, Env, Config) ->
    MoveAgent = fun (Pos, E) ->
                        model:move(Pos, E, Config)
                end,
    lists:foldl(MoveAgent, Env, Positions).

-spec shuffle(list()) -> list().
shuffle(L) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].
