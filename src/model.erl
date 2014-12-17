-module(model).

-export([initial_population/4,
         get_agent_char/2,
         move/3]).

-type ant_state() :: parallant:ant_state().

-include("parallant.hrl").

%% callbacks

-callback initial_population(PopulationSize :: pos_integer(),
                             Width :: dimension(),
                             Height :: dimension(),
                             Config :: config()) ->
    [{position(), ant_state()}].


-callback move(position(), environment(), config()) ->
    environment().

-callback get_agent_char(ant_state(), config()) ->
    char().

%% API

-spec initial_population(PopulationSize :: pos_integer(),
                         Width :: dimension(),
                         Height :: dimension(),
                         Config :: config()) ->
                                [{position(), ant_state()}].
initial_population(PopulationSize, Width, Height, Config) ->
    Model = get_model(Config),
    Model:initial_population(PopulationSize, Width, Height, Config).

-spec move(position(), environment(), config()) -> environment().
move(Pos, E, Config) ->
    Model = get_model(Config),
    Model:move(Pos, E, Config).

-spec get_agent_char(ant_state(), config()) -> char().
get_agent_char(State, Config) ->
    Model = get_model(Config),
    Model:get_agent_char(State, Config).

%% internal

-spec get_model(config()) -> model().
get_model(Config) ->
    Config#config.model.
