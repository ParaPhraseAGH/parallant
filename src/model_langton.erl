-module(model_langton).
-behaviour(model).

-include("parallant.hrl").

-export([initial_population/3,
         move/3,
         log_custom/3,
         get_agent_char/2]).

-type cell_state() :: dead | alive.
-type cell() :: {cell_state()}.
-type direction() :: north | south | east | west.
-type langton_agent_state() :: {direction(), cell()}.
-type agent_state() :: parallant:agent_state(langton_agent_state()).
-type move() :: {0, 1} | {1, 0} | {0, -1} | {-1, 0}.

%% model specific functions
-spec initial_population(PopulationSize :: pos_integer(),
                         World :: world(),
                         Config :: config()) ->
                                [{position(), agent_state()}].
initial_population(PopulationSize, World, Config) ->
    #world{w = Width, h = Height, d = _Depth} = World,
    K = 1,
    AllPositions = [{I, J, K} || I <- lists:seq(1, Width),
                                 J <- lists:seq(1, Height)],
    ShuffledPositions = algorithm:shuffle(AllPositions),
    AgentPositions = lists:sublist(ShuffledPositions, 1, PopulationSize),
    CellPositions = AllPositions,
    All = [{Pos, [agent]} || Pos <- AgentPositions]
        ++ [{Pos, [cell]} || Pos <- CellPositions],
    [populate_cell(Pos, Members, Config)
     || {Pos, Members} <- agents_lists:group_by(All)].

populate_cell(Pos, Members, _Config) ->
    AgentState = case lists:member(agent, Members) of
                     true ->
                         random_agent_state();
                     _ ->
                         empty
                 end,
    {Pos, {AgentState, initial_cell_state()}}.

-spec initial_cell_state() -> cell().
initial_cell_state() ->
    {dead}.

-spec random_agent_state() -> direction().
random_agent_state() ->
    random_direction().

-spec move(position(), environment(), config()) -> environment().
move(Position, E, Config) ->
    %% based on agent state and its neighbourhood
    %% compute the new agent state and neighbourhood
    %% langton's ant
    A = #agent{pos = Position, state = agents:get_agent(Position, E, Config)},
    {Old, New} = get_move(A, E, Config),
    #agent{pos = OPos, state = {ODir, OCell}} = Old,
    #agent{pos = NPos, state = {NDir, _}} = New,
    case {ODir, agents:get_agent(New#agent.pos, E, Config)} of
        {empty, _} ->
            E;
        {_, {empty, CellState}} ->
            E1 = agents:update_agent(NPos, {NDir, CellState}, E, Config),
            OldState = {empty, update_cell(OCell)},
            agents:update_agent(OPos, OldState, E1, Config);
        {_, empty} ->
            io:format("no agent on pos: ~p~n", [Position]),
            E;
        {_, _} ->
            E
    end.

-spec get_move(agent(), environment(), config()) -> {agent(), agent()}.
get_move(A, E, Config) ->
    New = move_agent(A, E, Config),
    {A#agent{state = New#agent.state}, New}.

-spec update_cell(cell()) -> cell().
update_cell({dead}) -> {alive};
update_cell({alive}) -> {dead}.

-spec move_agent(agent(), environment(), config()) -> agent().
move_agent(Agent = #agent{state = {empty, _Cell}}, #env{}, _C) ->
    Agent;
move_agent(#agent{pos = Pos, state = {Dir, {Cell}}}, #env{world = World}, _C) ->
    NewDir = turn(Dir, Cell),
    NewPos = forward(Pos, NewDir, World),
    #agent{pos = NewPos, state = {NewDir, {Cell}}}.


-spec forward(position(), direction(), world()) -> position().
forward({X, Y, Z}, Dir, #world{w = W, h = H}) ->
    {DX, DY} = heading(Dir),
    NewX = torus_bounds(X + DX, W),
    NewY = torus_bounds(Y + DY, H),
    {NewX, NewY, Z}.

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

-spec get_agent_char(agent_state(), config()) -> char().
get_agent_char(empty, _Config) ->
    $$;
get_agent_char({empty, CellState}, _Config) ->
    cell_char(CellState);
get_agent_char({Dir, _}, _Config) ->
    agent_char(Dir).

-spec agent_char(direction()) -> char().
agent_char(west) -> $<;
agent_char(east) -> $>;
agent_char(north) -> $^;
agent_char(south) -> $v.

-spec cell_char(cell()) -> char().
cell_char({alive}) -> $o;
cell_char({dead}) -> $..


%% logging
-spec log_custom(Step :: pos_integer(), environment(), config()) -> ok.
log_custom(_Step, _Env, _Config) ->
    %% cast log_custom to logger
    %% count number of ants
    %% Agents = agents:get_list(_Env#env.agents, _Config),
    %% Ants = [A || A = #agent{state = {S, _}} <- Agents, S /= empty],
    %% io:format("Agents: ~p~n", [length(Ants)]),
    ok.
