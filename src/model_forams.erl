-module(model_forams).
-behaviour(model).

-include("parallant.hrl").

-export([initial_cell_state/0,
         random_ant_state/0,
         initial_population/4,
         get_move/3,
         move/3,
         update_cell/1]).

-type move() :: {0, 1} | {1, 0} | {0, -1} | {-1, 0}.
-type cell_state() :: dead | alive.

% copied from langton's ant model
-spec initial_population(PopulationSize :: pos_integer(),
                         Width :: dimension(),
                         Height :: dimension(),
                         Config :: config()) ->
                                [{position(), ant_state()}].
initial_population(PopulationSize, Width, Height, Config) ->
    AllPositions = [{I, J} || I <- lists:seq(1, Width),
                              J <- lists:seq(1, Height)],
    %% AntPositions = lists:sublist(shuffle(AllPositions),
    %%                              1,
    %%                              PopulationSize),
    AlgaePositions = lists:sublist(shuffle(AllPositions),
                                   1,
                                   PopulationSize * 1),
    CellPositions = AllPositions,
    All = %%[{Pos, [ant]} || Pos <- AntPositions] ++
        [{Pos, [cell]} || Pos <- CellPositions]
        ++ [{Pos, [alga]} || Pos <- AlgaePositions],
    [populate_cell(Pos, Members, Config)
     || {Pos, Members} <- ants:group_by(All)].

populate_cell(Pos, Members, _Config) ->
    InitialAlgaeLevel = 3,
    %% _AntState = case lists:member(ant, Members) of
    %%                true ->
    %%                    random_ant_state();
    %%                _ ->
    %%                    empty
    %%            end,
    AlgaState = case lists:member(alga, Members) of
                    true ->
                        InitialAlgaeLevel;
                    _ ->
                        0
                end,
    {Pos, {AlgaState}}.

-spec shuffle(list()) -> list().
shuffle(L) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].

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
    %% algae dispersion
    #env{world = #world{w = W, h = H}} = E,
    Pos = A#ant.pos,
    UpdateNeighbour =
        fun(NPos, Env) ->
                {Level} = ants_impl:get_agent(NPos, Env, Config),
                ants_impl:update_agent(NPos, {Level + 1}, Env, Config)
        end,

    State = ants_impl:get_agent(Pos, E, Config),
    NewEnv = case State of
                 {Level} when Level > 4 ->
                     Ns = neighbours_4(Pos, W, H),
                     E1 = lists:foldl(UpdateNeighbour, E, Ns),
                     ants_impl:update_agent(Pos,
                                            {Level - length(Ns)},
                                            E1,
                                            Config);
                 {Level} when Level =< 0 orelse Level >= 10 ->
                     E;
                 {Level} ->
                     ants_impl:update_agent(Pos, {Level+1}, E, Config)
             end,
    NewEnv.

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

neighbours_4({X,Y}, W, H) ->
    [{torus_bounds(X+1, W), Y},
     {torus_bounds(X-1, W), Y},
     {X, torus_bounds(Y+1, H)},
     {X, torus_bounds(Y-1, H)}].


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
