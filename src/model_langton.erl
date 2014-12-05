-module(model_langton).
-behaviour(model).

-include("parallant.hrl").

-export([initial_population/4,
         move/3,
         get_agent_char/2]).

-type move() :: {0, 1} | {1, 0} | {0, -1} | {-1, 0}.
-type cell_state() :: dead | alive.

% model specific functions
-spec initial_population(PopulationSize :: pos_integer(),
                         Width :: dimension(),
                         Height :: dimension(),
                         Config :: config()) ->
                                [{position(), ant_state()}].
initial_population(PopulationSize, Width, Height, Config) ->
    AllPositions = [{I, J} || I <- lists:seq(1, Width),
                              J <- lists:seq(1, Height)],
    AntPositions = lists:sublist(shuffle(AllPositions), 1, PopulationSize),
    CellPositions = AllPositions,
    All = [{Pos, [ant]} || Pos <- AntPositions]
        ++ [{Pos, [cell]} || Pos <- CellPositions],
    [populate_cell(Pos, Members, Config)
     || {Pos, Members} <- ants:group_by(All)].

populate_cell(Pos, Members, _Config) ->
    AntState = case lists:member(ant, Members) of
                true ->
                    random_ant_state();
                _ ->
                    empty
            end,
    {Pos, {AntState, initial_cell_state()}}.

-spec shuffle(list()) -> list().
shuffle(L) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].

-spec initial_cell_state() -> cell().
initial_cell_state() ->
    {dead}.

-spec random_ant_state() -> ant_state().
random_ant_state() ->
    random_direction().

-spec move(position(), environment(), config()) -> environment().
move(Position, E, Config) ->
    %% based on agent state and its neighbourhood
    %% compute the new agent state and neighbourhood
    %% langton's ant
    A = #ant{pos = Position, state = ants_impl:get_agent(Position, E, Config)},
    {Old, New} = get_move(A, E, Config),
    #ant{pos = OPos, state = {ODir, OCell}} = Old,
    #ant{pos = NPos, state = {NDir, _}} = New,
    case {ODir, ants_impl:get_agent(New#ant.pos, E, Config)} of
        {empty, _} ->
            E;
        {_, {empty, CellState}} ->
            E1 = ants_impl:update_agent(NPos, {NDir, CellState}, E, Config),
            OldState = {empty, update_cell(OCell)},
            ants_impl:update_agent(OPos, OldState, E1, Config);
        {_, _} ->
            E
    end.

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

%% displaying agents

-spec get_agent_char(ant_state(), config()) -> char().
get_agent_char(empty, _Config) ->
    $$;
get_agent_char({empty, CellState}, _Config) ->
    cell_char(CellState);
get_agent_char({Dir, _}, _Config) ->
    ant_char(Dir).

-spec ant_char(ant_state()) -> char().
ant_char(west) -> $<;
ant_char(east) -> $>;
ant_char(north) -> $^;
ant_char(south) -> $v.

-spec cell_char(cell()) -> char().
cell_char({alive}) -> $o;
cell_char({dead}) -> $.;
cell_char({I}) -> I;
cell_char(_V) -> _V.
