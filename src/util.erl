%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2014 3:29 PM
%%%-------------------------------------------------------------------
-module(util).
-author("piotr").

-include("parallant.hrl").

%% API
-export([random_direction/0, shuffle/1, all_positions/2]).


-spec all_positions(dimension(), dimension()) -> [position()].
all_positions(Width, Height) ->
  [{I,J} || I <- lists:seq(1, Width), J <-lists:seq(1,Height)].

-spec shuffle(list()) -> list().
shuffle(L) ->
  [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].

-spec random_direction() -> direction().
random_direction() ->
  Dirs = [{0, 1}, {1, 0}, {0, -1}, {-1, 0}],
  Idx = random:uniform(length(Dirs)),
  lists:nth(Idx, Dirs).
