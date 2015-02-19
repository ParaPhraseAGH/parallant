%%%-------------------------------------------------------------------
%%% @author Daniel Grzonka
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% Agents ETS based approach
%%% @end
%%% Created : 28. lis 2014 13:31
%%%-------------------------------------------------------------------
-module(agents_ets).
-author("Daniel").

%% API
-export([create_agents/3,
         partition/3,
         update_agent/4,
         get_agent/3,
         get_positions/2]).

-include("parallant.hrl").

-define(LOAD(Attribute, Proplist, Default),
        Attribute = proplists:get_value(Attribute, Proplist, Default)).

-type agent_state() :: parallant:agent_state().
-type tile() :: agents:tile({Start :: dimension(), End :: dimension()}).

-spec create_agents(PopulationSize :: pos_integer(),
                    World :: world(),
                    config()) ->
                           TableID :: ets:tid().
create_agents(PopulationSize, World, Config) ->
    TableName = agentsETS,
    clean(TableName),
    TID = ets:new(TableName, [ordered_set, public, {keypos, #agent.pos}]),
    Pop = model:initial_population(PopulationSize, World, Config),
    Agents = [#agent{pos = Pos, state = State} || {Pos, State} <- Pop],
    ets:insert(TID, Agents),
    TID.

-spec get_agent(position(), environment(), config()) ->
                       agent_state().
get_agent(Pos, Env, _Config) ->
    case ets:lookup(Env#env.agents, Pos) of
        [] ->
            empty;
        [A] ->
            A#agent.state
    end.

-spec update_agent(position(), agent_state(), environment(), config()) ->
                          environment().
update_agent(Position, empty, Env, _Config) ->
    ets:delete(Env#env.agents, Position),
    Env;
update_agent(Position, NewState, Env, _Config) ->
    ets:insert(Env#env.agents, #agent{pos = Position, state = NewState}),
    Env.

-spec group_by_colour([[agent()]], pos_integer()) -> [[agent()]].
group_by_colour(Tiles, N) ->
    N = 2,
    EveryNth = fun (Rest) ->
                       [A || {I, A} <- lists:zip(lists:seq(1, length(Tiles)),
                                                 Tiles),
                             I rem N == Rest]
               end,
    lists:map(EveryNth, [I rem N || I <- lists:seq(1, N)]).


-spec partition(environment(),
                Colours :: pos_integer(),
                Parts :: pos_integer()) ->
                       [[{tile(), [agent()]}]].
partition(Env, 1, 1) ->
    [[{unique, ets:tab2list(Env#env.agents)}]];
partition(Env, NColours, NParts) ->
    W = (Env#env.world)#world.w,
    %% H = 5,
    D = round(W/(NParts*NColours)),

    Zeros = [{I, Env#env.agents} || I <- lists:seq(1, W, D)],
    TilesZero = [{{I, I+D-1}, T} || {I, T} <- Zeros],
    Colours = group_by_colour(TilesZero, NColours),
    Colours.


-spec get_positions(agents(), tile()) -> [position()].
get_positions(Agents, Tile) ->
    {F, T} = Tile,
    Positions = ets:select(Agents, [{explicit_ant_record(),
                                     [{'and',
                                       {'>=', '$1', F},
                                       {'=<', '$1', T}}],
                                     [{{'$1', '$2', '$3'}}]}]),
    Positions.

%% fool dialyzer in 17.0
-spec explicit_ant_record() -> tuple().
explicit_ant_record() ->
    list_to_tuple([agent, {'$1', '$2', '$3'}, '_']).

-spec clean(atom()) -> ok | error.
clean(TableId) ->
    try ets:delete(TableId) of
        true -> ok
    catch
        _:_ -> error
    end.
