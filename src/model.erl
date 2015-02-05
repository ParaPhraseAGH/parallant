-module(model).

-export([initial_population/3,
         get_agent_char/2,
         move/3]).

-type agent_state() :: parallant:agent_state().

-include("parallant.hrl").

%% callbacks

-callback initial_population(PopulationSize :: pos_integer(),
                             World :: world(),
                             Config :: config()) ->
    [{position(), agent_state()}].


-callback move(position(), environment(), config()) ->
    environment().

-callback get_agent_char(agent_state(), config()) ->
    char().

%% API

-spec initial_population(PopulationSize :: pos_integer(),
                         World :: world(),
                         Config :: config()) ->
                                [{position(), agent_state()}].
initial_population(PopulationSize, World, Config) ->
    Model = get_model(Config),
    Model:initial_population(PopulationSize, World, Config).

-spec move(position(), environment(), config()) -> environment().
move(Pos, E, Config) ->
    Model = get_model(Config),
    Model:move(Pos, E, Config).

-spec get_agent_char(agent_state(), config()) -> char().
get_agent_char(State, Config) ->
    Model = get_model(Config),
    Model:get_agent_char(State, Config).

%% internal

-spec get_model(config()) -> model().
get_model(Config) ->
    Config#config.model.
