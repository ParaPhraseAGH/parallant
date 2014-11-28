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
-export([create_ants/4, apply_move/3, partition/3]).

-include("parallant.hrl").

-define(LOAD(Attribute, Proplist, Default),
        Attribute = proplists:get_value(Attribute, Proplist, Default)).

-spec create_ants(pos_integer(), dimension(), dimension(), config()) -> atom().
create_ants(PopulationSize, Width, Height, Config) ->
    TID = ets:new(antsETS, [ordered_set, protected, {keypos, 2}]), %% unnamed ETS
    AllPositions = [{I, J} || I <- lists:seq(1, Width),
                              J <- lists:seq(1, Height)],
    ShuffledCellPositions = shuffle(AllPositions),
    AntPositions = lists:sublist(ShuffledCellPositions, 1, PopulationSize),
    ets:insert(TID, [#ant{pos = Pos, state = model:random_ant_state(Config)} ||
                        Pos <- lists:sort(AntPositions)]),
    TID.


-spec apply_move({ant(), ant()}, environment(), config()) -> environment().
apply_move({Old, New}, E, Config) ->
    case ets:lookup(E#env.agents, New#ant.pos) of
        [] ->
            ets:insert(E#env.agents, New),
            ets:delete(E#env.agents, Old),
            update_cell(Old#ant.pos, E, Config);
        [{ant, Pos, State}] when is_atom(State), is_tuple(Pos)->
            E
    end.

-spec update_cell(position(), environment(), config()) -> environment().
update_cell(Pos, E = #env{world = World}, Config) ->
    E#env{world = world_impl:update_cell(Pos, World, Config)}.


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


-spec partition(environment(), pos_integer(), pos_integer()) -> [[ant()]].
partition(Env, 1, 1) ->
    [ets:tab2list(Env#env.agents)]; %added: Conversion into list
partition(Env, NColours, NParts) ->
    W = (Env#env.world)#world.w,
    %% H = 5,
    D = round(W/NParts),
    Zeros = [{I, []} || I <- lists:seq(1, W, D)],
    AssignTileToAnt = fun(A = #ant{pos={X, _}}) ->
                              ITile = trunc((X-1)/D)*D+1,
                              {ITile, [A]}
                      end,
    TiledAnts = lists:map(AssignTileToAnt, ets:tab2list(Env#env.agents)), %added: Conversion into list
    TagTiles = group_by(TiledAnts ++ Zeros),
    Tiles = [T || {_, T} <- TagTiles],
    Colours = group_by_colour(Tiles, NColours),
    Colours.


-spec shuffle(list()) -> list().
shuffle(L) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].