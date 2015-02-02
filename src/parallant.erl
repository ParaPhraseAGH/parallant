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

-export_type([agent_state/0, agent_state/1]).

-type agent_state(Any) :: Any.
-type agent_state() :: empty | agent_state(any()).

-include("parallant.hrl").

-define(LOAD(Attribute, Proplist, Default),
        Attribute = proplists:get_value(Attribute, Proplist, Default)).

-spec test(Width :: dimension(), Height :: dimension(),
           NumberOfAgents :: pos_integer(), Steps :: pos_integer()) -> ok.
test(Width, Height, NAgents, Steps) ->
    Seed = erlang:now(),
    %%     io:format("Algorithm_seq:~n"),
    %%     test(algorithm_seq, Seed, Width, Height, NAgents, Steps, false).
    io:format("Algorithm_tiled:~n"),
    test(algorithm_tiled, Seed, Width, Height, NAgents, Steps, false).

-spec test() -> ok.
test() ->
    test(50, 30, 5, 500).

-spec test(algorithm()) -> ok.
test(Algorithm) ->
    Seed = erlang:now(),
    test(Algorithm, Seed, 50, 30, 5, 500, false).

-spec test(algorithm(), Seed :: any(), Width :: dimension(),
           Height :: dimension(), NumberOfAgents :: pos_integer(),
           Steps :: pos_integer(), Log :: boolean()) -> ok.
test(Algorithm, Seed, Width, Height, NAgents, Steps, Log) ->
    io:format("ListBasedAgentsImpl:~n"),
    random:seed(Seed),
    start(Width, Height, NAgents, Steps, [{algorithm, Algorithm},
                                          {agents, agents},
                                          {log, Log}]),
    io:format("Gb_treeBasedAgentsImpl:~n"),
    random:seed(Seed),
    start(Width, Height, NAgents, Steps, [{algorithm, Algorithm},
                                          {agents, agents_gbt},
                                          {log, Log}]),
    io:format("ETSBasedAgentsImpl:~n"),
    random:seed(Seed),
    start(Width, Height, NAgents, Steps, [{algorithm, Algorithm},
                                          {agents, agents_ets},
                                          {log, Log}]).


-spec start(Width :: dimension(), Height :: dimension(),
            Steps :: pos_integer()) -> ok.
start(Width, Height, Steps) ->
    start(Width, Height, 1, Steps, []).

-spec start(Width :: dimension(), Height :: dimension(),
            PopulationSize :: pos_integer(), Steps :: pos_integer(),
            ConfigOptions :: proplists:proplist()) -> ok.
start(Width, Height, PopulationSize, Steps, ConfigOptions) ->
    Config = create_config(ConfigOptions),

    World = create_world(Width, Height, Config),
    Agents = create_agents(PopulationSize, World, Config),
    Env = #env{agents = Agents,
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


%% internal functions

create_agents(PopSize, World, Config) ->
    agents:create_agents(PopSize, World, Config).

create_world(W, H, _Config)->
    #world{w = W, h = H, d = 1}.

create_config(ConfigProps) ->
    #config{?LOAD(model, ConfigProps, model_langton),
            ?LOAD(algorithm, ConfigProps, algorithm_seq),
            ?LOAD(agents, ConfigProps, agents_lists),
            ?LOAD(log, ConfigProps, true),
            ?LOAD(animate, ConfigProps, true),
            ?LOAD(tiles_per_colour, ConfigProps, 4),
            ?LOAD(workers_per_colour, ConfigProps, 4)}.
