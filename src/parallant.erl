%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2014 10:29 AM
%%%-------------------------------------------------------------------
-module(parallant).
%% API
-export([test/0, test/1, test/4, start/5, start/6]).
-export([get_cell/3, update_board/3, move_ants/4, update_cell/1]).

-include("parallant.hrl").

-spec test(dimension(), dimension(), pos_integer(), pos_integer()) -> ok.
test(Width, Height, NAnts, Steps) ->
    Seed = erlang:now(),
    io:format("Parallant_seq:~n"),
    test(parallant_seq, Seed, Width, Height, NAnts, Steps),
    io:format("Parallant_tiled:~n"),
    test(parallant_tiled, Seed, Width, Height, NAnts, Steps).

-spec test() -> ok.
test() ->
    test(50, 30, 5, 500).

-spec test(model()) -> ok.
test(Model) ->
    Seed = erlang:now(),
    test(Model, Seed, 50, 30, 5, 500).

-spec test(model(), any(), dimension(), dimension(), pos_integer(), pos_integer()) -> ok.
test(Model, Seed, Width, Height, NAnts, Steps) ->
    io:format("ListBased:~n"),
    random:seed(Seed),
    start(Model, list_based, Width, Height, NAnts, Steps),
    io:format("Gb_treeBased:~n"),
    random:seed(Seed),
    start(Model, gbtree_based, Width, Height, NAnts, Steps).

-spec start(model(), world_impl(), dimension(), dimension(), pos_integer()) -> ok.
start(Model, Impl, Width, Height, Steps) ->
    start(Model, Impl, Width, Height, 1, Steps).

-spec start(model(), world_impl(), dimension(), dimension(), pos_integer(), pos_integer()) -> ok.
start(Model, Impl, Width, Height, PopulationSize, Steps) ->
    Board = create_board(Impl, Width, Height),
    Ants = create_ants(Impl, PopulationSize, Width, Height),

    Env = #env{agents = Ants, world = Board, backend = Impl},

    Log = true,
    Animate = true,

    logger:start(Model, Env, Log, Animate),
    T1 = erlang:now(),

    EndEnv = Model:run(Steps, Env),

    T2 = erlang:now(),
    logger:stop(EndEnv),

    Time = timer:now_diff(T2, T1),
    TimeInSecs = Time / 1000000,
    io:format("Time elapsed: ~p. Time per iteration: ~p s~n", [TimeInSecs, TimeInSecs / Steps]).

-spec create_ants(pos_integer(), dimension(), dimension()) -> [ant()].
create_ants(PopulationSize, Width, Height) ->
    ShuffledCellPositions = util:shuffle(util:all_positions(Width, Height)),
    AntPositions = lists:sublist(ShuffledCellPositions, 1, PopulationSize),
    [#ant{pos = Pos, dir = util:random_direction()} || Pos <- AntPositions].

-spec move_ants([cell()], [ant()], world(), position()) -> [ant()].
move_ants([], [], _, _) -> [];
move_ants([AntCell | TAntCells], [Ant | TAnts], World, Occuppied) ->
    NewAnt = move_ant(AntCell, Ant, World, Occuppied),
    [NewAnt | move_ants(TAntCells, TAnts, World, [NewAnt#ant.pos | Occuppied])].

-spec update_board(world_impl(), world(), [ant()]) -> world().
update_board(Impl, World, Ants) ->
    Impl:update_board(World, Ants).

-spec update_cell(cell()) -> cell().
update_cell({dead}) -> {alive};
update_cell({alive}) -> {dead}.

move_ant({AntCellState}, #ant{pos = Pos, dir = Dir}, #world { w = W, h = H}, Occuppied) ->
    NewDir = turn(Dir, AntCellState),
    NewPos = forward(Pos, NewDir, W, H, Occuppied),
    #ant{pos = NewPos, dir = NewDir}.

forward({X, Y}, Dir, W, H, Occuppied) ->
    {DX, DY} = heading(Dir),
    NewX = torus_bounds(X + DX, W),
    NewY = torus_bounds(Y + DY, H),
    case lists:member({NewX, NewY}, Occuppied) of
        true -> {X, Y};
        false -> {NewX, NewY}
    end.

torus_bounds(Val, Max) when Val < 1 -> Max + Val;
torus_bounds(Val, Max) when Val > Max -> Val - Max;
torus_bounds(Val, _Max) -> Val.

turn(Dir, dead) -> turn_right(Dir);
turn(Dir, alive) -> turn_left(Dir).

turn_right(north) -> east;
turn_right(east) -> south;
turn_right(south) -> west;
turn_right(west) -> north.

turn_left(north) -> west;
turn_left(east) -> north;
turn_left(south) -> east;
turn_left(west) -> south.

heading(north) -> {0, 1};
heading(south) -> {0, -1};
heading(east) -> {1, 0};
heading(west) -> {-1, 0}.

create_ants(_Impl, PopSize, W, H) ->
    create_ants(PopSize, W, H).

create_board(Impl, W, H)->
    Board = Impl:create_board(W, H),
    #world{board = Board, w = W, h = H}.

-spec get_cell(world_impl(), position(), world()) -> cell().
get_cell(Impl, {X,Y}, World) ->
    Impl:get_cell({X,Y}, World).
