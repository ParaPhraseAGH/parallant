-module(agents).

-export([create_agents/3,
         get_agent/3,
         update_agent/4,
         get_positions/3,
         neighbourhood/3,
         get_list/2,
         get_tile/3,
         update_tiles/3]).

-export_type([tile/1, tile/0, agents/0, agents/1]).

-type agent_state() :: parallant:agent_state().

-type agents(Any) :: Any.
-type agents() :: agents(any()).
-type tile(Any) :: Any.
-type tile() :: tile(any()).
-type range() :: {position(), position()}.

-include("parallant.hrl").


-callback create_agents(pos_integer(), world(), config()) ->
    agents().

-callback get_agent(position(), environment(), config()) ->
    agent_state().

-callback update_agent(position(), agent_state(), environment(), config()) ->
    environment().

-callback get_positions(agents(), range()) ->
    [position()].

-callback get_list(agents()) ->
    [agent()].

-callback get_tile(range(), environment()) ->
    {range(), agents()}.

-callback update_tiles([environment()], environment(), config()) ->
    environment().

-spec create_agents(PopulationSize :: pos_integer(),
                    World :: world(),
                    config()) -> agents().
create_agents(PopulationSize, World, Config) ->
    Impl = Config#config.agents,
    Impl:create_agents(PopulationSize, World, Config).

-spec get_agent(position(), environment(), config()) -> agent_state().
get_agent(Position, Env, Config) ->
    Impl = Config#config.agents,
    Impl:get_agent(Position, Env, Config).

-spec update_agent(position(), agent_state(), environment(), config()) ->
                          environment().
update_agent(Position, NewState, Env, Config) ->
    Impl = Config#config.agents,
    Impl:update_agent(Position, NewState, Env, Config).

-spec neighbourhood(tile(), environment(), config()) -> [position()].
neighbourhood(Tile, Env, _Config) ->
    neighbourhood(Tile, Env).

%% internal functions

-spec get_impl(config()) -> agents_impl().
get_impl(Config) ->
    Config#config.agents.

-spec neighbourhood(tile(), environment()) -> [position()].
neighbourhood(Tile, #env{world = #world{w = W, h = H, d = D}}) ->
    {{Start, _, _}, {End, _, _}} = Tile,
    R = 1,
    Xs = lists:seq(Start, End) ++ [torus_bounds(Start - R, W),
                                   torus_bounds(End + R, W)],
    [{I, J, K} || I <- Xs, J <- lists:seq(1, H), K <- lists:seq(1, D)].

torus_bounds(X, Max) when X > Max ->
    X - Max;
torus_bounds(X, Max) when X < 1 ->
    X + Max;
torus_bounds(X, _Max) ->
    X.

-spec get_positions(agents(), range(), config()) -> [position()].
get_positions(Agents, Range, Config) ->
    Impl = get_impl(Config),
    Impl:get_positions(Agents, Range).

-spec get_list(agents(), config()) -> [agent()].
get_list(Agents, Config) ->
    Impl = get_impl(Config),
    Impl:get_list(Agents).

-spec get_tile(range(), environment(), config()) ->
                      {range(), agents()}.
get_tile(Range, Env, Config) ->
    Impl = get_impl(Config),
    Impl:get_tile(Range, Env).

-spec update_tiles([environment()], environment(), config()) -> environment().
update_tiles(NewEnvs, E, Config) ->
    Impl = get_impl(Config),
    Impl:update_tiles(NewEnvs, E, Config).
