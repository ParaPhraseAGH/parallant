-module(ants).

-include("parallant.hrl").

-behaviour(ants_impl).

-export([create_ants/4, apply_move/3, partition/3]).

-spec create_ants(pos_integer(), dimension(), dimension(), config()) -> [ant()].
create_ants(PopulationSize, Width, Height, Config) ->
    Pop = model_langton:initial_population(PopulationSize,
                                           Width,
                                           Height,
                                           Config),
    [#ant{pos = Pos, state = State} || {Pos, State} <- Pop].

get_agent(Position, Env, _Config) ->
    hd([State || #ant{pos = Pos, state = State} <- Env#env.agents,
                 Pos == Position]).

update_agent(Position, NewState, Env, _Config) ->
    Update = fun (A = #ant{pos = Pos})
                   when Pos == Position ->
                     A#ant{state = NewState};
                 (A) -> A
             end,
    Env#env{agents = lists:map(Update, Env#env.agents)}.


-spec apply_move({ant(), ant()}, environment(), config()) -> environment().
apply_move({Same, Same}, E, _Config) ->
    E;
apply_move({Old, New}, E, Config) ->
    io:format("Old: ~p, New: ~p~n", [Old, New]),
    IsPosTaken = fun(#ant{pos = P, state = {Dir, _Cell}}) ->
                         P == New#ant.pos andalso Dir /= empty
                 end,
    case lists:any(IsPosTaken, E#env.agents) of
        true ->
            io:format("true~n"),
            E;
        false ->
            io:format("false~n"),
            update_cells(Old, New, E, Config)
    end.

-spec update_cells(ant(), ant(), environment(), config()) -> environment().
update_cells(Old, New, E = #env{agents = Agents}, Config) ->
    #ant{pos = OPos, state = {_ODir, _OCell}} = Old,
    #ant{pos = NPos, state = {NDir, _NCell}} = New,
    FlipCell = fun (A = #ant{pos = APos, state = {_Dir, Cell}})
                     when APos == OPos ->
                       NewCell = model:update_cell(Cell, Config),
                       A#ant{state = {empty, NewCell}};
                   (A = #ant{pos = APos, state = {_Dir, Cell}})
                     when APos == NPos ->
                       A#ant{state = {NDir, Cell}};
                   (A) ->
                       A
               end,
    E#env{agents = lists:map(FlipCell, Agents)}.


-spec partition(environment(), pos_integer(), pos_integer()) -> [[ant()]].
partition(Env, 1, 1) ->
    [Env#env.agents];
partition(Env, NColours, NParts) ->
    W = (Env#env.world)#world.w,
    %% H = 5,
    D = round(W/NParts),
    Zeros = [{I, []} || I <- lists:seq(1, W, D)],
    AssignTileToAnt = fun(A = #ant{pos={X, _}}) ->
                              ITile = trunc((X-1)/D)*D+1,
                              {ITile, [A]}
                      end,
    TiledAnts = lists:map(AssignTileToAnt, Env#env.agents),
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
