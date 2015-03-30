-module(algorithm).

-export([run/3,
         shuffle/1,
         move_all/3,
         log_custom/3]).

-include("parallant.hrl").


-callback run(Steps, environment(), config()) -> environment() when
      Steps :: pos_integer().

%% Exported functions

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


%% custom log every custom_log_interval iterations
-spec log_custom(log_step(), environment(), config()) -> ok.
log_custom(_, _, C) when (C#config.custom_log_interval == 0) orelse
                         (C#config.custom_log_interval == off) ->
    ok;
log_custom(Step, Env, C) when Step == starting orelse Step == ending ->
    model:log_custom(Step, Env, C);
log_custom(Step, Env, C) when Step rem C#config.custom_log_interval == 0 ->
    model:log_custom(Step, Env, C);
log_custom(_Step, _Env, _Config) ->
    ok.
