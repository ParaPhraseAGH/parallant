-module(ants).

-include("parallant.hrl").

-compile(export_all).

-spec apply_move({ant(), ant()}, environment()) -> environment().
apply_move({Old, New}, E) ->
    IsPosTaken = fun(#ant{pos = P}) -> P == New#ant.pos end,
    case lists:any(IsPosTaken, E#env.agents) of
        true ->
            E;
        false ->
            NewAgents = [A || A <- E#env.agents, A#ant.pos /= Old#ant.pos],
            update_cell(Old#ant.pos, E#env{agents = [New | NewAgents]})
    end.

-spec update_cell(position(), environment()) -> environment().
update_cell(Pos, E = #env{backend = Impl, world = World}) ->
    E#env{world = world_impl:update_cell(Impl, Pos, World)}.


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
                       lists:flatten(
                         [A || {I, A} <- lists:zip(lists:seq(1, length(Tiles)),
                                                   Tiles),
                               I rem N == Rest])
               end,
    lists:map(EveryNth, [I rem N || I <- lists:seq(1, N)]).
