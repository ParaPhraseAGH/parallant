%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(algorithm_skel).

-behaviour(algorithm).
%% API
-export([run/3]).

-export([test_temp/0]).

-type tile() :: agents:tile().
-type agents() :: agents:agents().

-include("parallant.hrl").

test_temp() ->
    parallant:start(104,100,1,100,40,
                    [{algorithm,?MODULE},
                     {model,model_langton},
                     {agents,agents_ets},
                     {custom_log_interval,10},
                     {log_world,false},
                     {tiles_per_colour,4},
                     {workers_per_colour,4}]).


-spec run(Steps :: pos_integer(), environment(), config()) -> environment().
run(Steps, Env, Config) when Config#config.agents =:= agents_ets ->
    step(1, Steps, Env, Config).

-spec step(Iteration :: pos_integer(),
           MaxIteraions :: pos_integer(),
           environment(),
           config()) -> environment().
step(Iteration, MaxIteraions, Env, _Config) when Iteration =:= MaxIteraions ->
    Env;
step(Iteration, MaxIteraions, Enviroment, Config) ->

    Workers = Config#config.workers_per_colour,

    FirstColour = {seq, fun (Env) ->
                                first_colour(Env, Config)
                        end},

    SendToWork = {map, [{seq,
                         fun ({Tile, Env}) ->
                                 NewTile = process_tile(Tile, Env, Config),
                                 {NewTile, Env}
                         end}],
                  Workers},

    MergeToEnv = {seq, fun (Tiles) ->
                               merge_tiles(Tiles, Config)
                       end},
    SecondColour = {seq, fun (Env) ->
                                 second_colour(Env, Config)
                         end},

    [NewEnv] = skel:do(_Workflow = [FirstColour,
                                    SendToWork,
                                    MergeToEnv,
                                    SecondColour,
                                    SendToWork,
                                    MergeToEnv],
                       _Input = [Enviroment]),

    logger:log(NewEnv),

    step(Iteration+1, MaxIteraions, NewEnv, Config).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Skel work-functions


-spec first_colour(environment(), config()) -> list({tile(), environment()}).
first_colour(Env, C) ->
    NCoulours = 2,
    NParts = C#config.tiles_per_colour,
    Colours = partition(Env, NCoulours, NParts, C),
    [First, _Second] = Colours,
    lists:map(fun(Tile) ->
                      {Tile, Env}
              end, First).

-spec process_tile(tile(), environment(), config()) -> tile().
process_tile(Tile, Env, Confing) ->
    io:format("process Tile ~p,~n Env: ~p  ~n",[Tile, Env]),
    %% TODO move all here
    Tile.


merge_tiles(Tiles, _Config) ->
    %% no merging needed in ETS implementation
    {_Tile, Env} = hd(Tiles),
    Env.


-spec second_colour(environment(), config()) -> list({tile(), environment()}).
second_colour(Env, C) ->
    NCoulours = 2,
    NParts = C#config.tiles_per_colour,
    Colours = partition(Env, NCoulours, NParts, C),
    [_First, Second] = Colours,
    lists:map(fun(Tile) ->
                      {Tile, Env}
              end, Second).










%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Old Code to be thrown away ......


-spec partition(environment(),
                Colours :: pos_integer(),
                Parts :: pos_integer(),
                config()) ->
                       [[{tile(), agents()}]].

partition(Env, NColours, NParts, Config) ->
    W = (Env#env.world)#world.w,
    H = (Env#env.world)#world.h,
    D = (Env#env.world)#world.d,
    %% H = 5,
    Dist = round(W/(NParts*NColours)),
    TagTiles = agents:get_tiles(Dist, Env, Config),
    Tiles = [{{{I, 1, 1}, {I+Dist-1, H, D}}, T} || {I, T} <- TagTiles],
    Colours = group_by_colour(Tiles, NColours),
    Colours.

-spec group_by_colour([[agent()]], pos_integer()) -> [[agent()]].
group_by_colour(Tiles, N) ->
    N = 2,
    EveryNth = fun (Rest) ->
                       [A || {I, A} <- lists:zip(lists:seq(1, length(Tiles)),
                                                 Tiles),
                             I rem N == Rest]
               end,
    lists:map(EveryNth, [I rem N || I <- lists:seq(1, N)]).

 
