-module(ants_impl).

-export([create_ants/4, partition/4, get_agent/3, update_agent/4]).

-include("parallant.hrl").


-callback create_ants(pos_integer(), dimension(), dimension(), config()) ->
    [ant()].

-callback get_agent(position(), environment(), config()) ->
    ant_state().

-callback update_agent(position(), ant_state(), environment(), config()) ->
    environment().

-callback partition(environment(),
                    Colours :: pos_integer(),
                    Parts :: pos_integer()) ->
    [[ant()]].


-spec create_ants(pos_integer(), dimension(), dimension(), config()) -> [ant()].
create_ants(PopulationSize, Width, Height, Config) ->
    Impl = Config#config.ants_impl,
    Impl:create_ants(PopulationSize, Width, Height, Config).

-spec get_agent(position(), environment(), config()) -> ant_state().
get_agent(Position, Env, Config) ->
    Impl = Config#config.ants_impl,
    Impl:get_agent(Position, Env, Config).

-spec update_agent(position(), ant_state(), environment(), config()) ->
                          environment().
update_agent(Position, NewState, Env, Config) ->
    Impl = Config#config.ants_impl,
    Impl:update_agent(Position, NewState, Env, Config).

-spec partition(environment(), pos_integer(), pos_integer(), config()) ->
                       [[ant()]].
partition(Env, NColours, NParts, Config) ->
    Impl = Config#config.ants_impl,
    Impl:partition(Env, NColours, NParts).
