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
-export([test/0, display/5, run/6]).

-include("parallant.hrl").

-spec test() -> ok.
test() ->
  parallant:test(?MODULE).

-spec display(world_impl(), [ant()], board(), dimension(), dimension()) -> ok.
display(Impl, Ants, Board, Width, Height) ->
  Impl:display(Ants, Board, Width, Height).

-spec run(world_impl(), [cell()], dimension(), dimension(), [ant()], pos_integer()) -> {[cell()],[ant()]}.
run(Impl, Board, W, H, Ants, Steps) ->
  step(Impl, Board, W, H, Ants, 1, Steps).

-spec step(world_impl(), [cell()], dimension(), dimension(), [ant()], pos_integer(), pos_integer()) -> {[cell()],[ant()]}.
step(_Impl, Board, _W, _H, Ants, MaxT, MaxT) ->
  {Board, Ants};
step(Impl, Board, W, H, Ants, T, MaxT) ->
  AntCells = [parallant:get_cell(Impl, APos, W, H, Board) || #ant{pos = APos} <- Ants],
  NewAnts = parallant:move_ants(AntCells, Ants, W, H, []),
  NewBoard = parallant:update_board(Impl, Board, W, H, Ants),
  parallant:log(?MODULE, Impl, NewAnts, NewBoard, T+1, W, H),
  step(Impl, NewBoard, W, H, NewAnts, T + 1, MaxT).