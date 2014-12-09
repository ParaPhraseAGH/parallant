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
-export([move_all/2]).

-export_type([ant_state/0, ant_state/1]).

-type ant_state(Any) :: Any.
-type ant_state() :: empty | ant_state(any()).

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
                                        {log, Log}]),
    io:format("Gb_treeBased:~n"),
    random:seed(Seed),
    start(Width, Height, NAnts, Steps, [{algorithm, Algorithm},
                                        {log, Log}]).

-spec start(dimension(), dimension(), pos_integer()) -> ok.
start(Width, Height, Steps) ->
    start(Width, Height, 1, Steps, []).

-spec start(dimension(), dimension(), pos_integer(), pos_integer(),
            proplists:proplist()) -> ok.
start(Width, Height, PopulationSize, Steps, ConfigOptions) ->
    Config = create_config(ConfigOptions),

    World = create_world(Width, Height, Config),
    Ants = create_ants(PopulationSize, Width, Height, Config),
    Env = #env{agents = Ants,
               world = World},

    logger:start(Env, Config),
    T1 = erlang:now(),

    EndEnv = algorithm:run(Steps, Env, Config),

    T2 = erlang:now(),
    logger:stop(EndEnv),

    Time = timer:now_diff(T2, T1),
    TimeInSecs = Time / 1000000,
    io:format("Time elapsed: ~p. Time per iteration: ~p s~n",
              [TimeInSecs, TimeInSecs / Steps]).

-spec move_all(environment(), config()) -> environment().
move_all(Env, Config) ->
    MoveAgent = fun (Agent, E) ->
                        model:move(Agent#ant.pos, E, Config)
                end,
    lists:foldl(MoveAgent, Env, Env#env.agents).

% internal functions

create_ants(PopSize, W, H, Config) ->
    ants_impl:create_ants(PopSize, W, H, Config).

create_world(W, H, _Config)->
    #world{w = W, h = H}.

create_config(ConfigProps) ->
    #config{?LOAD(model, ConfigProps, model_langton),
            ?LOAD(algorithm, ConfigProps, parallant_seq),
            ?LOAD(ants_impl, ConfigProps, ants),
            ?LOAD(log, ConfigProps, true),
            ?LOAD(animate, ConfigProps, true)}.
