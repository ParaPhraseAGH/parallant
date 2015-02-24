-module(algorithm).

-export([test/1, run/3, shuffle/1, move_all/3, partition/4]).

-include("parallant.hrl").

-type tile(Any) :: Any.
-type tile() :: tile(any()).
-type agents(Any) :: Any.
-type agents() :: agents(any()).

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


-spec group_by_colour([[agent()]], pos_integer()) -> [[agent()]].
group_by_colour(Tiles, N) ->
    N = 2,
    EveryNth = fun (Rest) ->
                       [A || {I, A} <- lists:zip(lists:seq(1, length(Tiles)),
                                                 Tiles),
                             I rem N == Rest]
               end,
    lists:map(EveryNth, [I rem N || I <- lists:seq(1, N)]).


-spec partition(environment(),
                Colours :: pos_integer(),
                Parts :: pos_integer(),
                config()) ->
                       [[{tile(), agents()}]].

partition(Env, 1, 1, Config) ->
    [[{unique, agents:get_list(Env#env.agents, Config)}]];
partition(Env, NColours, NParts, Config) ->
    W = (Env#env.world)#world.w,
    H = (Env#env.world)#world.h,
    D = (Env#env.world)#world.d,
    %% H = 5,
    Dist = round(W/(NParts*NColours)),
    TagTiles = agents:get_tiles(Dist, Env, Config),
    Tiles = [{{{I, 1, 1}, {I+Dist-1, H, D}}, T} || {I, T} <- TagTiles],
    Colours = group_by_colour(Tiles, NColours),
    Colours.