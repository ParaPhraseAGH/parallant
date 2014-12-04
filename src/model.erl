-module(model).

-include("parallant.hrl").

-export([initial_cell_state/1,
         random_ant_state/1,
         initial_population/4,
         get_move/3,
         move/3,
         update_cell/2]).

% callbacks

-callback get_move(ant(), environment(), config()) ->
    {ant(), ant()}.

-callback initial_population(PopulationSize :: pos_integer(),
                             Width :: dimension(),
                             Height :: dimension(),
                             Config :: config()) ->
    [{position(), ant_state()}].

-callback update_cell(cell()) ->
     cell().

-callback move(ant(), environment(), config()) ->
    environment().


-spec initial_cell_state(config()) -> cell().
initial_cell_state(Config) ->
    Model = Config#config.model,
    Model:initial_cell_state().

-spec random_ant_state(config()) -> ant_state().
random_ant_state(Config) ->
    Model = Config#config.model,
    Model:random_ant_state().

-spec update_cell(cell(), config()) -> cell().
update_cell(CellState, Config) ->
    Model = Config#config.model,
    Model:update_cell(CellState).

-spec get_move(ant(), environment(), config()) -> {ant(), ant()}.
get_move(A, E, Config) ->
    Model = Config#config.model,
    Model:get_move(A, E, Config).

-spec initial_population(PopulationSize :: pos_integer(),
                         Width :: dimension(),
                         Height :: dimension(),
                         Config :: config()) ->
                                [{position(), ant_state()}].
initial_population(PopulationSize, Width, Height, Config) ->
    Model = Config#config.model,
    Model:initial_population(PopulationSize, Width, Height, Config).

-spec move(ant(), environment(), config()) -> environment().
move(A, E, Config) ->
    Model = Config#config.model,
    Model:move(A, E, Config).
