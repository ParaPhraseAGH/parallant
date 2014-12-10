%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(parallant_tiled).

-behaviour(algorithm).
%% API
-export([test/0, run/3, poolboy_transaction/5]).

-include("parallant.hrl").

-spec test() -> ok.
test() ->
    parallant:test(?MODULE).

-spec run(pos_integer(), environment(), config()) -> environment().
run(Steps, Env, Config) ->
    {ok, Pool} = poolboy:start([{worker_module, tile_worker},
                                {size, 4}, % TODO from config.n _parts
                                {max_overflow, 4}]),
    step(1, Steps, Env, Pool, Config).

-spec step(pos_integer(), pos_integer(), environment(), poolboy:pool(),
           config()) -> environment().
step(MaxT, MaxT, Env, _Pool, _Config) ->
    Env;
step(T, MaxT, Env, Pool, Config) ->
    NColours = 2,
    NParts = 2, % TODO move to config
    Partitioned = ants_impl:partition(Env, NColours, NParts, Config),

    ProcessTile =
        fun(Tile, E) ->
                SendToWork = fun(Agents) ->
                                     send_to_work(Pool, Agents, E, Config)
                             end,
                lists:map(SendToWork, Tile),
                UpdatedEnvs = collect_results(Tile),
                %% io:format("~nTile: ~p~n",[Tile]),
                %% io:format("~nUpdated: ~p~n",[UpdatedEnvs]),
                %% apply updated environments for every agent
                %% in the tile and its neighbourhood
                %% parallant:apply_moves(Moves, E, Config)
                E
        end,
    NewEnv = lists:foldl(ProcessTile, Env, Partitioned),
    logger:log(NewEnv),
    step(T+1, MaxT, NewEnv, Pool, Config).

send_to_work(Pool, Agents, Env, Config) ->
    proc_lib:spawn_link(?MODULE,
                        poolboy_transaction,
                        [Pool, Agents, _Caller = self(), Env, Config]).

-spec poolboy_transaction(poolboy:pool(),
                          [ant()],
                          pid(),
                          environment(),
                          config()) -> any().
poolboy_transaction(Pool, Agents, Caller, Env, Config) ->
    %% TODO do we need transaction at this point?
    poolboy:transaction(Pool, mk_worker(Caller, Agents, Env, Config)).


mk_worker(Caller, {Tile, Agents}, Env, Config) ->
    fun (Worker) ->
            Shuffled = shuffle(Agents),
            Result = tile_worker:move_all(Worker,
                                          {Tile, Shuffled},
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

-spec shuffle(list()) -> list().
shuffle(L) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].
