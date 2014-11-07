-module(model).

-include("parallant.hrl").

-export([get_move/2, update_cell/1]).

-spec update_cell(cell()) -> cell().
update_cell({dead}) -> {alive};
update_cell({alive}) -> {dead}.

-spec get_move(ant(), environment()) -> {ant(), ant()}.
get_move(A, E) ->
    New = move_agent(A, E),
    {A#ant{dir = New#ant.dir}, New}.

-spec move_agent(ant(), environment()) -> ant().
move_agent(#ant{pos = Pos, dir = Dir}, #env{backend = Impl, world = World}) ->
    {AgentCellState} = world_impl:get_cell(Impl, Pos, World),
    NewDir = turn(Dir, AgentCellState),
    NewPos = forward(Pos, NewDir, World),
    #ant{pos = NewPos, dir = NewDir}.


-spec forward(position(), direction(), world()) -> position().
forward({X, Y}, Dir, #world{w = W, h = H}) ->
    {DX, DY} = heading(Dir),
    NewX = torus_bounds(X + DX, W),
    NewY = torus_bounds(Y + DY, H),
    {NewX, NewY}.

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