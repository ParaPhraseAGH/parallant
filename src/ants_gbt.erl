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
%-export([]).
-compile(export_all).

-include("parallant.hrl").

create_ants(PopulationSize, Width, Height) ->
  ShuffledCellPositions = util:shuffle(util:all_positions(Width, Height)),
  AntPositions = lists:sublist(ShuffledCellPositions, 1, PopulationSize),
  gb_trees:from_orddict([{Pos, #ant{pos = Pos, dir = util:random_direction()}} || Pos <- AntPositions]). % {Pozycja, CałyAgent} - ew. do zmiany, jest zbalansowany [DG]
  %io:format("~p", [AntPositions]),
  %AntsListWithKeys=lists:zip(lists:seq(1, PopulationSize), [#ant{pos = Pos, dir = util:random_direction()} || Pos <- AntPositions]),
  %io:format("~p", [AntsListWithKeys]),
  %gb_trees:from_orddict(AntsListWithKeys). %balanced tree

-spec apply_move({ant(), ant()}, environment()) -> environment().
apply_move({Old, New}, E) ->
  case gb_trees:lookup(New#ant.pos, E#env.agents) of
    none ->
      %io:format("New: ~p~n Old: ~p~n", [New, Old]),
      AgentsAfterInsert = gb_trees:insert(New#ant.pos, New, E#env.agents),
      %io:format("AgentsAfterInsert: ~p~n", [AgentsAfterInsert]),
      %io:format("Keys: ~p~n", [gb_trees:keys(AgentsAfterInsert)]),
      %io:format("OldPos: ~p~n", [Old#ant.pos]),
      %{value, OldVal} = gb_trees:lookup(Old#ant.pos,AgentsAfterInsert),
      %io:format("OldVal: ~p~n", [OldVal]),

      AgentsAfterDelete = gb_trees:delete(Old#ant.pos, AgentsAfterInsert),
      %io:format("AgentsAfterDelete: ~p~n", [AgentsAfterDelete]),
      update_cell(Old#ant.pos, E#env{agents = AgentsAfterDelete});
    {value, _} ->
      E
  end.

-spec update_cell(position(), environment()) -> environment().
update_cell(Pos, E = #env{backend = Impl, world = World}) ->
  E#env{world = world_impl:update_cell(Impl, Pos, World)}.



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
  [gb_trees:values(Env#env.agents)]; %added: Conversion into list [DG]
partition(Env, NColours, NParts) ->
  W = (Env#env.world)#world.w,
  %% H = 5,
  D = round(W/NParts),
  Zeros = [{I, []} || I <- lists:seq(1, W, D)],
  AssignTileToAnt = fun(A = #ant{pos={X, _}}) ->
    ITile = trunc((X-1)/D)*D+1,
    {ITile, [A]}
  end,
  TiledAnts = lists:map(AssignTileToAnt, gb_trees:values(Env#env.agents)), %added: Conversion into list [DG]
  TagTiles = group_by(TiledAnts ++ Zeros),
  Tiles = [T || {_, T} <- TagTiles],
  Colours = group_by_colour(Tiles, NColours),
  Colours.


%% Poszukiwanie wartości
%% is_value(Iter, SeekingValue) ->
%%   case gb_trees:next(Iter) of
%%     {Key, Value, Iter2} ->
%%       if
%%         Value =:= SeekingValue ->
%%           true;
%%         true ->
%%           is_value(Iter2, SeekingValue)
%%       end;
%%     none -> false
%%   end.



test() ->
  T=create_ants(2, 5, 5),
  Board = create_world(5, 5, []),
  Env = #env{agents = T,
    world = Board,
    backend = gbtree_based},
  A1 = {ant,{1,1},south},
  {_,A2} = gb_trees:smallest(T),
  apply_move({A2, A1}, Env).


create_world(W, H, _)->
  Board = world_impl:create_board(gbtree_based, W, H),
  #world{board = Board, w = W, h = H}.

reload() ->
code:purge(ants_gbt),
code:soft_purge(ants_gbt),
{module, ants_gbt} = code:load_file(ants_gbt),
{ok, ants_gbt}.
