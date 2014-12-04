-module(model_forams).
-behaviour(model).

-include("parallant.hrl").

-export([initial_cell_state/0,
         random_ant_state/0,
         get_move/3,
         move/3,
         update_cell/1]).

-type move() :: {0, 1} | {1, 0} | {0, -1} | {-1, 0}.
-type cell_state() :: dead | alive.

% copied from langton's ant model

-spec initial_cell_state() -> cell().
initial_cell_state() ->
    {dead}.

-spec random_ant_state() -> ant_state().
random_ant_state() ->
    random_direction().

-spec move(ant(), environment(), config()) -> environment().
move(A, E, Config) ->
    %% based on agent state and its neighbourhood
    %% compute the new agent state and neighbourhood
    %% langton's ant
    M = get_move(A, E, Config),
    ants_impl:apply_move(M, E, Config).

-spec get_move(ant(), environment(), config()) -> {ant(), ant()}.
get_move(A, E, Config) ->
    New = move_agent(A, E, Config),
    {A#ant{state = New#ant.state}, New}.

-spec update_cell(cell()) -> cell().
update_cell({dead}) -> {alive};
update_cell({alive}) -> {dead}.

-spec move_agent(ant(), environment(), config()) -> ant().
move_agent(Ant = #ant{state = {empty, _Cell}}, #env{}, _C) ->
    Ant;
move_agent(#ant{pos = Pos, state = {Dir, {Cell}}}, #env{world = World}, _C) ->
    %% {AgentCellState} = world_impl:get_cell(C#config.world_impl, Pos, World),
    NewDir = turn(Dir, Cell),
    NewPos = forward(Pos, NewDir, World),
    #ant{pos = NewPos, state = {NewDir, {Cell}}}.


-spec forward(position(), direction(), world()) -> position().
forward({X, Y}, Dir, #world{w = W, h = H}) ->
    {DX, DY} = heading(Dir),
    NewX = torus_bounds(X + DX, W),
    NewY = torus_bounds(Y + DY, H),
    {NewX, NewY}.

torus_bounds(Val, Max) when Val < 1 -> Max + Val;
torus_bounds(Val, Max) when Val > Max -> Val - Max;
torus_bounds(Val, _Max) -> Val.

-spec turn(direction(), cell_state()) -> direction().
turn(Dir, dead) -> turn_right(Dir);
turn(Dir, alive) -> turn_left(Dir).

-spec turn_right(direction()) -> direction().
turn_right(north) -> east;
turn_right(east) -> south;
turn_right(south) -> west;
turn_right(west) -> north.

-spec turn_left(direction()) -> direction().
turn_left(north) -> west;
turn_left(east) -> north;
turn_left(south) -> east;
turn_left(west) -> south.

-spec heading(direction()) -> move().
heading(north) -> {0, 1};
heading(south) -> {0, -1};
heading(east) -> {1, 0};
heading(west) -> {-1, 0}.

-spec random_direction() -> direction().
random_direction() ->
    Dirs = [north, south, east, west],
    Idx = random:uniform(length(Dirs)),
    lists:nth(Idx, Dirs).
