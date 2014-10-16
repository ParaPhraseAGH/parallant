%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(parallant_seq).

%% API
-export([test/0, display/1, run/2]).

-include("parallant.hrl").

-spec test() -> ok.
test() ->
    parallant:test(?MODULE).

-spec display(environment()) -> ok.
display(E) ->
    (E#env.backend):display(E#env.agents, E#env.world).

-spec run(pos_integer(), environment()) -> environment().
run(Steps, Env) ->
    step(1, Steps, Env).

-spec step(pos_integer(), pos_integer(), environment()) -> environment().
step(MaxT, MaxT, Env) ->
    Env;
step(T, MaxT, E = #env{backend = Impl, agents = Ants, world = World}) ->
    #world{w = W, h = H} = World,
    AntCells = [parallant:get_cell(Impl, APos, World) || #ant{pos = APos} <- Ants],
    NewAnts = parallant:move_ants(AntCells, Ants, World, []),
    NewBoard = parallant:update_board(Impl, World, Ants),
    parallant:log(?MODULE, Impl, NewAnts, NewBoard, T+1, W, H),
    step(T + 1, MaxT, E#env{world = NewBoard, agents = NewAnts}).
