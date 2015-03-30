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
-export([run/3,
         poolboy_transaction/5]).

-type tile() :: agents:tile().
-type agents() :: agents:agents().

-include("parallant.hrl").


-spec run(Steps :: pos_integer(), environment(), config()) -> environment().
run(Steps, Env, Config) ->
    {ok, Pool} = poolboy:start([{worker_module, tile_worker},
                                {size, Config#config.workers_per_colour},
                                {max_overflow, 4}]),
    step(1, Steps, Env, Pool, Config).

-spec step(T :: pos_integer(), MaxIteraions :: pos_integer(),
           environment(), poolboy:pool(), config()) -> environment().
step(Iteration, MaxIteraions, Env, _Pool, _Config) when Iteration =:= MaxIteraions ->
    Env;
step(Iteration, MaxIteraions, Env, Pool, Config) ->
    NColours = 2,
    NParts = Config#config.tiles_per_colour,
    Partitioned = partition(Env, NColours, NParts, Config),
    ProcessColour =
        fun(Colour, E) ->
                SendToWork = fun(Agents) ->
                                     send_to_work(Pool, Agents, E, Config)
                             end,
                lists:map(SendToWork, Colour),
                NewEnvs = collect_results(Colour),
                agents:update_tiles(NewEnvs, E, Config)
        end,
    NewEnv = lists:foldl(ProcessColour, Env, Partitioned),
    logger:log(NewEnv),
    algorithm:log_custom(Iteration, NewEnv, Config),
    step(Iteration+1, MaxIteraions, NewEnv, Pool, Config).


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

send_to_work(Pool, Agents, Env, Config) ->
    proc_lib:spawn_link(?MODULE,
                        poolboy_transaction,
                        [Pool, Agents, _Caller = self(), Env, Config]).

-spec poolboy_transaction(poolboy:pool(),
                          {tile(), agents()},
                          pid(),
                          environment(),
                          config()) -> any().
poolboy_transaction(Pool, Agents, Caller, Env, Config) ->
    %% TODO do we need transaction at this point?
    poolboy:transaction(Pool, mk_worker(Caller, Agents, Env, Config)).


mk_worker(Caller, {Tile, Agents}, Env, Config) ->
    fun (Worker) ->
            %% Result = Env
            Result = tile_worker:move_all(Worker,
                                          {Tile, Agents},
                                          Env,
                                          Config),
            Caller ! {agents, {Tile, Result}}
    end.

collect_results(Args) ->
    lists:map(fun (_) ->
                      receive
                          {agents, R} -> R
                      end
              end, Args).
