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
-export([create_agents/4, partition/3, update_agent/4, get_agent/3]).

-include("parallant.hrl").

-define(LOAD(Attribute, Proplist, Default),
        Attribute = proplists:get_value(Attribute, Proplist, Default)).

-type agent_state() :: parallant:agent_state().
-type tile() :: agents:tile({Start :: dimension(), End :: dimension()}).

-spec create_agents(PopulationSize :: pos_integer(),
                    Width :: dimension(),
                    Height :: dimension(),
                    config()) ->
                           TableID :: ets:tid().
create_agents(PopulationSize, Width, Height, Config) ->
    TID = ets:new(agentsETS, [ordered_set, public, {keypos, #agent.pos}]),
    Pop = model:initial_population(PopulationSize, Width, Height, Config),
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


-spec group_by([{term(), [term()]}]) -> [{term(), [term()]}].
group_by(List) ->
    dict:to_list(
      lists:foldl(fun({K, V}, D) ->
                          dict:append_list(K, V, D)
                  end, dict:new(), List)).

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
    D = round(W/NParts),
    Zeros = [{I, []} || I <- lists:seq(1, W, D)],
    AssignTileToAgent = fun(A = #agent{pos = {X, _}}) ->
                                ITile = trunc((X-1)/D)*D+1,
                                {ITile, [A]}
                        end,
    TiledAgents = lists:map(AssignTileToAgent, ets:tab2list(Env#env.agents)),
    TagTiles = group_by(TiledAgents ++ Zeros),
    Tiles = [{{I, I+D-1}, T} || {I, T} <- TagTiles],
    Colours = group_by_colour(Tiles, NColours),
    Colours.
