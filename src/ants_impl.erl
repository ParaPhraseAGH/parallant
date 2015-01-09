-module(ants_impl).

-export([create_ants/4, partition/4, get_agent/3, update_agent/4]).
-export([neighbourhood/3]).

-export_type([tile/1, tile/0]).

-type ant_state() :: parallant:ant_state().
-type tile(Any) :: Any.
-type tile() :: tile(any()).

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
    [[{tile(), [ant()]}]].

%% -callback neighbourhood(tile(), environment()) ->
    %% [position()].

-spec create_ants(PopulationSize :: pos_integer(), Width :: dimension(),
                  Height :: dimension(), config()) -> [ant()].
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

-spec partition(environment(), NumberOfColours :: pos_integer(),
                NumberOfParts :: pos_integer(), config()) -> [[ant()]].
partition(Env, NColours, NParts, Config) ->
    Impl = get_impl(Config),
    Impl:partition(Env, NColours, NParts).

-spec neighbourhood(tile(), environment(), config()) -> [position()].
neighbourhood(Tile, Env, _Config) ->
    %% Impl = get_impl(Config),
    neighbourhood(Tile, Env).

% internal functions

-spec get_impl(config()) -> ants_impl().
get_impl(Config) ->
    Config#config.ants_impl.

-spec neighbourhood(tile(), environment()) -> [position()].
neighbourhood(Tile, #env{world = #world{w = W, h = H}}) ->
    {Start, End} = Tile,
    R = 1,
    Xs = lists:seq(Start, End) ++ [torus_bounds(Start - R, W),
                                   torus_bounds(End + R, W)],
    [{I, J} || I <- Xs, J <- lists:seq(1, H)].

torus_bounds(X, Max) when X > Max ->
    X - Max;
torus_bounds(X, Max) when X < 1 ->
    X + Max;
torus_bounds(X, _Max) ->
    X.
