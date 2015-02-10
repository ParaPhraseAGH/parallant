-module(model_langton3d).
-behaviour(model).

-include("parallant.hrl").

-export([initial_population/3,
         move/3,
         get_agent_char/2]).

-type cell_state() :: dead | alive.
-type cell() :: {cell_state()}.
-type direction() :: pos_x | neg_x | pos_y | neg_y | pos_z | neg_z.
-type langton_rule() :: [direction()].
-type direction3d() :: {langton_rule(), direction(), langton_rule()}.
-type langton_agent_state() :: {direction3d(), cell()}.
-type agent_state() :: parallant:agent_state(langton_agent_state()).
-type move() :: {0, 1, 0} | {1, 0, 0} | {0, -1, 0} |
                {-1, 0, 0} | {0, 0, 1} | {0, 0, -1}.

%% model specific functions
-spec initial_population(PopulationSize :: pos_integer(),
                         World :: world(),
                         Config :: config()) ->
                                [{position(), agent_state()}].
initial_population(PopulationSize, World, Config) ->
    #world{w = Width, h = Height, d = Depth} = World,
    AllPositions = [{I, J, K} || I <- lists:seq(1, Width),
                                 J <- lists:seq(1, Height),
                                 K <- lists:seq(1, Depth)],
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

-spec ant_rule() -> langton_rule().
ant_rule() ->
    [pos_x, pos_y, pos_z, neg_x, neg_y, neg_z].

-spec initial_cell_state() -> cell().
initial_cell_state() ->
    {dead}.

-spec random_agent_state() -> direction3d().
random_agent_state() ->
    [H | T] = ant_rule(),
    {lists:reverse(ant_rule()), H, T}.
    %% {random_direction(), random_plane()}.

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
    New = turn(Dir, Cell),
    {_, NewDir, _} = New,
    NewPos = forward(Pos, NewDir, World),
    #agent{pos = NewPos, state = {New, {Cell}}}.


-spec forward(position(), direction(), world()) -> position().
forward({X, Y, Z}, Dir, #world{w = W, h = H, d = D}) ->
    {DX, DY, DZ} = direction_to_heading(Dir),
    NewX = torus_bounds(X + DX, W),
    NewY = torus_bounds(Y + DY, H),
    NewZ = torus_bounds(Z + DZ, D),
    {NewX, NewY, NewZ}.

torus_bounds(Val, Max) when Val < 1 -> Max + Val;
torus_bounds(Val, Max) when Val > Max -> Val - Max;
torus_bounds(Val, _Max) -> Val.

-spec turn(direction3d(), cell_state()) -> direction3d().
turn(Dir, dead) -> turn_left(Dir);
turn(Dir, alive) -> turn_right(Dir).

-spec turn_right(direction3d()) -> direction3d().
turn_right({_B1, _Dir, []}) ->
    [H | T] = ant_rule(),
    {lists:reverse(ant_rule()), H, T};
turn_right({B1, Dir, [H | T]}) -> {[Dir | B1], H, T}.

-spec turn_left(direction3d()) -> direction3d().
turn_left({[], _Dir, _B2}) ->
    [H | T] = lists:reverse(ant_rule()),
    {T, H, ant_rule()};
turn_left({[H | T], Dir, B2}) -> {T, H, [Dir | B2]}.

-spec direction_to_heading(direction()) -> move().
direction_to_heading(pos_x) -> {1, 0, 0};
direction_to_heading(neg_x) -> {-1, 0, 0};
direction_to_heading(pos_y) -> {0, 1, 0};
direction_to_heading(neg_y) -> {0, -1, 0};
direction_to_heading(pos_z) -> {0, 0, 1};
direction_to_heading(neg_z) -> {0, 0, -1}.

%% displaying agents

-spec get_agent_char(agent_state(), config()) -> char().
get_agent_char(empty, _Config) ->
    $$;
get_agent_char({empty, {CellState}}, _Config) ->
    cell_char(CellState);
get_agent_char({{_Buffer1, Dir, _Buffer2}, _}, _Config) ->
    agent_char(Dir).

-spec agent_char(direction()) -> char().
agent_char(neg_x) -> $<;
agent_char(pos_x) -> $>;
agent_char(pos_y) -> $^;
agent_char(neg_y) -> $v;
agent_char(pos_z) -> $x;
agent_char(neg_z) -> $*.

-spec cell_char(cell_state()) -> char().
cell_char(dead) -> $.;
cell_char(alive) -> $o.
