%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jun 2014 2:33 PM
%%%-------------------------------------------------------------------
-module(graphics).
-author("piotr").

-include("parallant.hrl").
%% API
-export([display/2]).

pos_to_index({X, Y}, W, H) ->
    (H - Y) * W + X.
%%   io:format("X:~p, Y:~p, W:~p, I:~p~n",[X,Y,W,I]),

split_into_chunks(List, ChunkLen) ->
    split_into_chunks(List, [], ChunkLen, 1).

split_into_chunks([], _ChunkAcc, _, _) ->
    [];
split_into_chunks([H | T], ChunkAcc, ChunkLen, ChunkLen) ->
    [lists:reverse([H | ChunkAcc]) | split_into_chunks(T, [], ChunkLen, 1)];
split_into_chunks([H | T], ChunkAcc, ChunkLen, CurrIndex) ->
    split_into_chunks(T, [H | ChunkAcc], ChunkLen, CurrIndex + 1).

display_cell(I, Cell, AntsWithIndex) ->
    Result = lists:keytake(I, 1, AntsWithIndex),
    {C, RestOfAnts} = case Result of
                          {value, {_I, #ant{dir = ADir}}, Rest} ->
                              {ant_char(ADir), Rest};
                          false ->
                              {cell_char(Cell), AntsWithIndex}
                      end,
    io:format("~c ", [C]),
    RestOfAnts.

%% @doc display/4 expects list of cells in the following (lexicographic) order:
%% [{1,1}, {1,2}, {1,3}, {1,4}, ... {1, H}, {2,1}, {2,2} ... {W,1}, {W,2}, ..., {W, H}]
%% and displays the board in a natural way:
%% ^ y (up to max=H)
%% |
%% |
%% *------> x (up to max=W)
%%
-spec display([ant()], board()) -> ok.
display(Ants, #world{board = Board, w = W, h = H}) ->
    AntsWithIndex = [{pos_to_index(A#ant.pos, W, H), A} || A <- Ants],
    ChunkedBoard = split_into_chunks(Board, H),
    TransposedBoard = lists:reverse(transpose_board(ChunkedBoard)),
    %%   io:format("RawBoard: ~p~n",[Board]),
    %%   io:format("ChunkedBoard: ~p~n",[ChunkedBoard]),
    %%   io:format("TransposedBoard: ~p~n",[TransposedBoard]),
    display(lists:keysort(1, AntsWithIndex), lists:flatten(TransposedBoard), W, H, 1).

transpose_board([[]|_]) -> [];
transpose_board(M) ->
    [lists:map(fun hd/1, M) | transpose_board(lists:map(fun tl/1, M))].

display(_Ants, [], _W, _H, _N) ->
    io:format("~n");
display(Ants, [HB | TB], W, H, I) when I rem W == 0 ->
    RestOfAnts = display_cell(I, HB, Ants),
    io:format("~n"),
    display(RestOfAnts, TB, W, H, I + 1);
display(Ants, [HB | TB], W, H, I) ->
    RestOfAnts = display_cell(I, HB, Ants),
    display(RestOfAnts, TB, W, H, I + 1).

-spec ant_char(direction()) -> char().
ant_char(west) -> $<;
ant_char(east) -> $>;
ant_char(north) -> $^;
ant_char(south) -> $v.

-spec cell_char(cell()) -> char().
cell_char({alive}) -> $o;
cell_char({dead}) -> $.;
cell_char({I}) -> I;
cell_char(_V) -> _V.
