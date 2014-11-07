%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(parallant).
%% API
-export([test/0, test/1, test/4, start/5, start/7]).
-export([get_cell/3, get_moves/1, apply_moves/2]).

-include("parallant.hrl").

-spec test(dimension(), dimension(), pos_integer(), pos_integer()) -> ok.
test(Width, Height, NAnts, Steps) ->
    Seed = erlang:now(),
    io:format("Parallant_seq:~n"),
    test(parallant_seq, Seed, Width, Height, NAnts, Steps),
    io:format("Parallant_tiled:~n"),
    test(parallant_tiled, Seed, Width, Height, NAnts, Steps).

-spec test() -> ok.
test() ->
    test(50, 30, 5, 500).

-spec test(model()) -> ok.
test(Model) ->
    Seed = erlang:now(),
    test(Model, Seed, 50, 30, 5, 500).

-spec test(model(), any(), dimension(), dimension(),
           pos_integer(), pos_integer()) -> ok.
test(Model, Seed, Width, Height, NAnts, Steps) ->
    io:format("ListBased:~n"),
    random:seed(Seed),
    start(Model, list_based, Width, Height, NAnts, Steps, true),
    io:format("Gb_treeBased:~n"),
    random:seed(Seed),
    start(Model, gbtree_based, Width, Height, NAnts, Steps, true).

-spec start(model(), world_impl(), dimension(), dimension(),
            pos_integer()) -> ok.
start(Model, Impl, Width, Height, Steps) ->
    start(Model, Impl, Width, Height, 1, Steps, true).

-spec start(model(), world_impl(), dimension(), dimension(),
            pos_integer(), pos_integer(), boolean()) -> ok.
start(Model, Impl, Width, Height, PopulationSize, Steps, Log) ->
    Board = create_world(Impl, Width, Height),
    Ants = create_ants(Impl, PopulationSize, Width, Height),
    Env = #env{agents = Ants, world = Board, backend = Impl},

    Animate = true,

    logger:start(Model, Env, Log, Animate),
    T1 = erlang:now(),

    EndEnv = Model:run(Steps, Env),

    T2 = erlang:now(),
    logger:stop(EndEnv),

    Time = timer:now_diff(T2, T1),
    TimeInSecs = Time / 1000000,
    io:format("Time elapsed: ~p. Time per iteration: ~p s~n",
              [TimeInSecs, TimeInSecs / Steps]).

-spec get_cell(world_impl(), position(), world()) -> cell().
get_cell(Impl, {X, Y}, World) ->
    world_impl:get_cell(Impl, {X, Y}, World).

-spec get_moves(environment()) -> [{Old :: ant(), New :: ant()}].
get_moves(E = #env{agents = Agents}) ->
    [model:get_move(A, E) || A <- Agents].

-spec apply_moves([{ant(), ant()}], environment()) ->
                         {[ant()], environment()}.
apply_moves(Moves, Env) ->
    ApplyMove = fun (Move, E) -> ants:apply_move(Move, E) end,
    lists:foldl(ApplyMove, Env, Moves).

% internal functions

create_ants(_Impl, PopSize, W, H) ->
    ants:create_ants(PopSize, W, H).

create_world(Impl, W, H)->
    Board = world_impl:create_board(Impl, W, H),
    #world{board = Board, w = W, h = H}.
