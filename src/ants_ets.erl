%%%-------------------------------------------------------------------
%%% @author Daniel Grzonka
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% Ants ETS based approach
%%% @end
%%% Created : 28. lis 2014 13:31
%%%-------------------------------------------------------------------
-module(ants_ets).
-author("Daniel").

%% API
-export([create_ants/4, partition/3, update_agent/4, get_agent/3]).

-include("parallant.hrl").

-define(LOAD(Attribute, Proplist, Default),
        Attribute = proplists:get_value(Attribute, Proplist, Default)).

-type ants() :: ants_impl:ants(ets:tid()).
-type ant_state() :: parallant:ant_state().
-type tile() :: ants_impl:tile({Start :: dimension(), End :: dimension()}).

-spec create_ants(PopulationSize :: pos_integer(),
                  Width :: dimension(),
                  Height :: dimension(),
                  config()) ->
                         TableID :: ants().
create_ants(PopulationSize, Width, Height, Config) ->
    TID = ets:new(antsETS, [ordered_set, public, {keypos, #ant.pos}]),
    Pop = model:initial_population(PopulationSize, Width, Height, Config),
    Agents=[#ant{pos = Pos, state = State} || {Pos, State} <- Pop],
    ets:insert(TID, Agents),
    TID.

-spec get_agent(position(), environment(), config()) ->
                       ant_state().
get_agent(Pos, Env, _Config) ->
    case ets:lookup(Env#env.agents, Pos) of
        [] ->
            empty;
        [A] ->
            A#ant.state
    end.

-spec update_agent(position(), ant_state(), environment(), config()) ->
                          environment().
update_agent(Position, empty, Env, _Config) ->
    ets:delete(Env#env.agents, Position),
    Env;
update_agent(Position, NewState, Env, _Config) ->
    ets:insert(Env#env.agents, #ant{pos=Position, state = NewState}),
    Env.


-spec group_by([{term(), [term()]}]) -> [{term(), [term()]}].
group_by(List) ->
    dict:to_list(
      lists:foldl(fun({K, V}, D) ->
                          dict:append_list(K, V, D)
                  end, dict:new(), List)).

-spec group_by_colour([[ant()]], pos_integer()) -> [[ant()]].
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
                       [[{tile(), [position()]}]].
partition(Env, 1, 1) ->
    [[{unique, select_positions(Env#env.agents)}]];
partition(Env, NColours, NParts) ->
    W = (Env#env.world)#world.w,
    %% H = 5,
    D = round(W/NParts),
    Zeros = [{I, []} || I <- lists:seq(1, W, D)],
    AssignTileToAnt = fun(Pos = {X, _}) ->
                              ITile = trunc((X-1)/D)*D+1,
                              {ITile, [Pos]}
                      end,
    TiledAnts = lists:map(AssignTileToAnt, select_positions(Env#env.agents)),
    TagTiles = group_by(TiledAnts ++ Zeros),
    Tiles = [{{I, I+D-1}, T} || {I, T} <- TagTiles],
    Colours = group_by_colour(Tiles, NColours),
    Colours.


-spec select_positions(ants()) -> [position()].
select_positions(TableId) ->
    ets:select(TableId,
               [{#ant{pos = '$1',
                      state = '_'},
                 [],
                 ['$1']}]).
