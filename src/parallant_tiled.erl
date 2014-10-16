%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(parallant_tiled).
%% API
-export([test/0, display/1, run/2]).

-include("parallant.hrl").

-ifdef(skel).
-define(TILED_MODULE, tiled_skel).
-else.
-define(TILED_MODULE, tiled).
-endif.
-define(N_WORKERS, 4).

-spec test() -> ok.
test() ->
    parallant:test(?MODULE).

-spec run(pos_integer(), environment()) -> environment().
run(Steps, E = #env{world = World, agents = Ants}) ->
    KColours = 2,
    NParts = ?N_WORKERS * KColours,
    {TiledAnts, TilesDict} =
        ?TILED_MODULE:split_ants_to_tiles(Ants, World, NParts),
    {EndEnv, EndTiledAnts} = step(1, Steps, E, TiledAnts, TilesDict),
    EndAnts = ?TILED_MODULE:flatten_tiles(EndTiledAnts),
    EndEnv#env{agents = EndAnts}.

-spec display(environment()) -> ok.
display(E = #env{agents = Ants}) when is_list(Ants) ->
    (E#env.backend):display(Ants, E#env.world);
display(E) ->
    FlattenedAnts = ?TILED_MODULE:flatten_tiles(E#env.agents),
    display(E#env{agents = FlattenedAnts}).


-spec step(CurrentStep, MaxStep, Env, TiledAnts, TilesDict) ->
                  {EndEnv, EndAnts} when
      Env :: environment(),
      TiledAnts :: dict:dict(pos_integer(), [ant()]),
      TilesDict :: dict:dict(pos_integer(), tile()),
      CurrentStep :: pos_integer(),
      MaxStep :: pos_integer(),
      EndAnts :: dict:dict(pos_integer(), [ant()]),
      EndEnv :: environment().

step(MaxStep, MaxStep, Env, TiledAnts, _TilesDict) ->
    {Env, TiledAnts};
step(Step, MaxStep, E = #env{world = World, backend = Impl}, Ants, TilesDict) ->
    KColours = 2,
    AntList = ?TILED_MODULE:flatten_tiles(Ants),
    NewWorld = parallant:update_board(Impl, World, AntList),
    NewAnts = ?TILED_MODULE:update_colours(KColours, Ants, TilesDict, E),
    NewEnv = E#env{world = NewWorld, agents = NewAnts},
    logger:log(?MODULE, NewEnv, Step),
    step(Step + 1, MaxStep, NewEnv, NewAnts, TilesDict).
