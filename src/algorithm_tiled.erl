%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(algorithm_tiled).

-behaviour(algorithm).
%% API
-export([test/0, run/3, poolboy_transaction/5]).
-type tile() :: agents:tile().
-type agents() :: agents:agents().

-include("parallant.hrl").

-spec test() -> ok.
test() ->
    parallant:test(?MODULE).

-spec run(Steps :: pos_integer(), environment(), config()) -> environment().
run(Steps, Env, Config) ->
    {ok, Pool} = poolboy:start([{worker_module, tile_worker},
                                {size, Config#config.workers_per_colour},
                                {max_overflow, 4}]),
    step(1, Steps, Env, Pool, Config).

-spec step(T :: pos_integer(), MaxT :: pos_integer(),
           environment(), poolboy:pool(), config()) -> environment().
step(MaxT, MaxT, Env, _Pool, _Config) ->
    Env;
step(T, MaxT, Env, Pool, Config) ->
  %io:format("Step ~p~n", [T]),
    NColours = 2,
    NParts = Config#config.tiles_per_colour,
    Partitioned = algorithm:partition(Env, NColours, NParts, Config),
    ProcessColour =
        fun(Colour, E) ->
                SendToWork = fun(Range) ->
                                     Tile=agents:get_tile(Range, E, Config),
                                     send_to_work(Pool, Tile, E, Config)
                             end,
                lists:map(SendToWork, Colour),
                NewEnvs = collect_results(Colour),
%%           NA = [{agent,Pos,{Dir,State}} || {agent,Pos,{Dir,State}} <- lists:flatten([Agents || {_, {env, Agents, _}} <- NewEnvs]), Dir=/=empty],
%%           io:format("Step ~p After processing one color: ~p~n", [T, NA]),
                NE = agents:update_tiles(NewEnvs, E, Config),
%%           {env, Agents, _} = NE,
%%           Test = [{agent,Pos,{Dir,State}} || {agent,Pos,{Dir,State}}  <- Agents, Dir=/=empty],
%%           io:format("Step ~p Agents after update: ~p~n", [T, Test]),
          NE
        end,
    NewEnv = lists:foldl(ProcessColour, Env, Partitioned),
%%   {env, Agents, _} = NewEnv,
%%   Test = [{agent,Pos,{Dir,State}} || {agent,Pos,{Dir,State}}  <- Agents, Dir=/=empty],
%%   io:format("Agents: ~p~n", [Test]),
    logger:log(NewEnv),
    step(T+1, MaxT, NewEnv, Pool, Config).


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
