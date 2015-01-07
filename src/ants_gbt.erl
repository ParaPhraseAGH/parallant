%%%-------------------------------------------------------------------
%%% @author Daniel Grzonka
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% Ants gb_trees based approach
%%% @end
%%% Created : 07. lis 2014 15:56
%%%-------------------------------------------------------------------
-module(ants_gbt).
-author("Daniel").

%% API
-export([create_ants/4, partition/3, get_agent/3, update_agent/4, group_by/1]).
-type ant_state() :: parallant:ant_state().

-include("parallant.hrl").

-spec create_ants(PopulationSize :: pos_integer(),
                  Width :: dimension(),
                  Height :: dimension(),
                  config()) ->
                         Ants :: gb_trees:tree().
create_ants(PopulationSize, Width, Height, Config) ->
    Pop = model:initial_population(PopulationSize, Width, Height, Config),
    IndividualsWithKeys = [{Pos,
                            #ant{pos = Pos, state = State}}
                           || {Pos, State} <- Pop],
    gb_trees:from_orddict(IndividualsWithKeys).

%% -spec get_agent(position(), environment(), config()) -> ant_state().
%% get_agent(Position, Env, _Config) ->
%% case gb_trees:lookup(Position,  Env#env.agents) of
%%   {value, Value} ->
%%     #ant{state = State} = Value,
%%     State;
%%   none ->
%%     empty
%% end.

-spec get_agent(position(), environment(), config()) -> ant_state().
get_agent(Position, Env, _Config) ->
    io:format("~n***********~n"),
    io:format("Position: ~p~n", [Position]),
    io:format("Agents: ~p~n", [gb_trees:values(Env#env.agents)]),
    io:format("AgentsTree: ~p~n", [Env#env.agents]),
    Filtered = [State || #ant{pos = Pos, state = State} <- gb_trees:values(Env#env.agents),
                         Pos == Position],
    TreeRes = gb_trees:lookup(Position,  Env#env.agents),
    io:format("ListRes: ~p~n", [Filtered]),
    io:format("TreeRes: ~p~n", [TreeRes]),
    case Filtered of
        [] ->
            empty;
        [Agent] ->
            Agent
    end.


-spec update_agent(position(), ant_state(), environment(), config()) ->
                          environment().
update_agent(Position, empty, Env, _Config) ->
    gb_trees:delete(Position, Env#env.agents);
update_agent(Position, NewState, Env, Config) ->
    case get_agent(Position, Env, Config) of
        empty ->
            NewAgent = #ant{pos = Position, state = NewState},
            NewAgents = gb_trees:insert(Position, NewAgent, Env#env.agents),
            Env#env{agents = NewAgents};
        _ ->
            NewAgents = gb_trees:update(Position, NewState, Env#env.agents),
            Env#env{agents = NewAgents}
    end.

-spec partition(environment(), pos_integer(), pos_integer()) -> [[ant()]].
partition(Env, 1, 1) ->
    [[{unique, gb_trees:values(Env#env.agents)}]];
partition(Env, NColours, NParts) ->
    W = (Env#env.world)#world.w,
    %% H = 5,
    D = round(W/NParts),
    Zeros = [{I, []} || I <- lists:seq(1, W, D)],
    AssignTileToAnt = fun(A = #ant{pos={X, _}}) ->
                              ITile = trunc((X-1)/D)*D+1,
                              {ITile, [A]}
                      end,
    TiledAnts = lists:map(AssignTileToAnt, gb_trees:values(Env#env.agents)),
    TagTiles = group_by(TiledAnts ++ Zeros),
    Tiles = [T || {_, T} <- TagTiles],
    Colours = group_by_colour(Tiles, NColours),
    Colours.

-spec group_by([{term(), [term()]}]) -> [{term(), [term()]}].
group_by(List) ->
    dict:to_list(
      lists:foldl(fun({K, V}, D) ->
                          dict:append_list(K, V, D)
                  end, dict:new(), List)).

-spec group_by_colour([[ant()]], pos_integer()) -> [[ant()]].
group_by_colour(Tiles, N) ->
    N = 2, % dividing in stripes
    EveryNth = fun (Rest) ->
                       [A || {I, A} <- lists:zip(lists:seq(1, length(Tiles)),
                                                 Tiles),
                             I rem N == Rest]
               end,
    lists:map(EveryNth, [I rem N || I <- lists:seq(1, N)]).
