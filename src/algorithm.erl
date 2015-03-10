-module(algorithm).

-export([test/1, run/3, shuffle/1, move_all/3, partition/4]).

-include("parallant.hrl").

-type tile(Any) :: Any.
-type tile() :: tile(any()).
-type agents(Any) :: Any.
-type agents() :: agents(any()).
-type range() :: {position(), position()}.

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


-spec group_by_colour([range()], pos_integer()) -> [[range()]].
group_by_colour(Ranges, N) ->
    N = 2,
    EveryNth = fun (Rest) ->
                       [A || {I, A} <- lists:zip(lists:seq(1, length(Ranges)),
                         Ranges),
                             I rem N == Rest]
               end,
    lists:map(EveryNth, [I rem N || I <- lists:seq(1, N)]).


-spec partition(environment(),
                Colours :: pos_integer(),
                Parts :: pos_integer(),
                config()) ->
                       [[range()]].

partition(Env, 1, 1, _Config) ->
    %[[{unique, agents:get_list(Env#env.agents, Config)}]];
  W = (Env#env.world)#world.w,
  H = (Env#env.world)#world.h,
  D = (Env#env.world)#world.d,
  [[{{1,1,1},{W,H,D}}]];
partition(Env, NColours, NParts, _Config) ->
    W = (Env#env.world)#world.w,
    H = (Env#env.world)#world.h,
    D = (Env#env.world)#world.d,
    %% H = 5,
    Dist = round(W/(NParts*NColours)),

  Ranges = [{{I, 1, 1}, {I+Dist-1, H, D}} || I <- lists:seq(1, W, Dist)],
  Colours=group_by_colour(Ranges, NColours),
%%io:format("Colours: ~p~n", [Colours]),
%%     TagTiles = agents:get_tiles(Dist, Env, Config),
%%     Tiles = [{{{I, 1, 1}, {I+Dist-1, H, D}}, T} || {I, T} <- TagTiles],
%%     Colours = group_by_colour(Tiles, NColours),
    Colours.