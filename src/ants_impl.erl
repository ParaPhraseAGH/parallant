-module(ants_impl).

-export([create_ants/4, apply_move/3, partition/4]).

-include("parallant.hrl").



-callback create_ants(pos_integer(), dimension(), dimension(), config()) ->
    [ant()].

-callback apply_move({ant(), ant()}, environment(), config()) ->
    environment().

-callback partition(environment(),
                    Colours :: pos_integer(),
                    Parts :: pos_integer()) ->
    [[ant()]].


-spec create_ants(PopulationSize :: pos_integer(), Width :: dimension(),
                  Height :: dimension(), config()) -> [ant()].
create_ants(PopulationSize, Width, Height, Config) ->
    Impl = Config#config.ants_impl,
    Impl:create_ants(PopulationSize, Width, Height, Config).

-spec apply_move({ant(), ant()}, environment(), config()) -> environment().
apply_move(Move, E, Config) ->
    Impl = Config#config.ants_impl,
    Impl:apply_move(Move, E, Config).

-spec partition(environment(), NumberOfColours :: pos_integer(),
                NumberOfParts :: pos_integer(), config()) -> [[ant()]].
partition(Env, NColours, NParts, Config) ->
    Impl = Config#config.ants_impl,
    Impl:partition(Env, NColours, NParts).
