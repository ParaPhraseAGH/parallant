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
-export([test/0, test/1, test/4, start/3, start/5]).
-export([get_cell/3, get_moves/2, apply_moves/3]).

-include("parallant.hrl").

-define(LOAD(Attribute, Proplist, Default),
        Attribute = proplists:get_value(Attribute, Proplist, Default)).

-spec test(dimension(), dimension(), pos_integer(), pos_integer()) -> ok.
test(Width, Height, NAnts, Steps) ->
    Seed = erlang:now(),
    io:format("Parallant_seq:~n"),
    test(parallant_seq, Seed, Width, Height, NAnts, Steps, false),
    io:format("Parallant_tiled:~n"),
    test(parallant_tiled, Seed, Width, Height, NAnts, Steps, false).

-spec test() -> ok.
test() ->
    test(50, 30, 5, 500).

-spec test(algorithm()) -> ok.
test(Algorithm) ->
    Seed = erlang:now(),
    test(Algorithm, Seed, 50, 30, 5, 500, false).

-spec test(algorithm(), any(), dimension(), dimension(),
           pos_integer(), pos_integer(), boolean()) -> ok.
test(Algorithm, Seed, Width, Height, NAnts, Steps, Log) ->
    io:format("ListBased:~n"),
    random:seed(Seed),
    start(Width, Height, NAnts, Steps, [{algorithm, Algorithm},
                                        {world_impl, list_based},
                                        {log, Log}]),
    io:format("Gb_treeBased:~n"),
    random:seed(Seed),
    start(Width, Height, NAnts, Steps, [{algorithm, Algorithm},
                                        {world_impl, gbtree_based},
                                        {log, Log}]).

-spec start(dimension(), dimension(), pos_integer()) -> ok.
start(Width, Height, Steps) ->
    start(Width, Height, 1, Steps, []).

-spec start(dimension(), dimension(), pos_integer(), pos_integer(),
            proplists:proplist()) -> ok.
start(Width, Height, PopulationSize, Steps, ConfigOptions) ->
    Config = create_config(ConfigOptions),

    Board = create_world(Width, Height, Config),
    Ants = create_ants(PopulationSize, Width, Height, Config),
    Env = #env{agents = Ants,
               world = Board},

    logger:start(Env, Config),
    T1 = erlang:now(),

    EndEnv = algorithm:run(Steps, Env, Config),

    T2 = erlang:now(),
    logger:stop(EndEnv),

    Time = timer:now_diff(T2, T1),
    TimeInSecs = Time / 1000000,
    io:format("Time elapsed: ~p. Time per iteration: ~p s~n",
              [TimeInSecs, TimeInSecs / Steps]).

-spec get_cell(world_impl(), position(), world()) -> cell().
get_cell(Impl, {X, Y}, World) ->
    world_impl:get_cell(Impl, {X, Y}, World).

-spec get_moves(environment(), config()) -> [{Old :: ant(), New :: ant()}].
get_moves(E = #env{agents = Agents}, Config) ->
    [model:get_move(A, E, Config) || A <- Agents].

-spec apply_moves([{ant(), ant()}], environment(), config()) -> environment().
apply_moves(Moves, Env, Config) ->
    ApplyMove = fun (Move, E) -> ants:apply_move(Move, E, Config) end,
    lists:foldl(ApplyMove, Env, Moves).

% internal functions

create_ants(PopSize, W, H, Config) ->
    %% Model = Config#config.model,
    ants:create_ants(PopSize, W, H, Config).

create_world(W, H, Config)->
    Board = world_impl:create_board(W, H, Config),
    #world{board = Board, w = W, h = H}.

create_config(ConfigProps) ->
    #config{?LOAD(model, ConfigProps, model),
            ?LOAD(algorithm, ConfigProps, parallant_seq),
            ?LOAD(world_impl, ConfigProps, gbtree_based),
            ?LOAD(ants_impl, ConfigProps, ants),
            ?LOAD(log, ConfigProps, true),
            ?LOAD(animate, ConfigProps, true)}.
