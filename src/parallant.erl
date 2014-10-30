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
-export([test/0, test/1, test/4, start/5, start/7]).
-export([get_cell/3, update_cell/1, get_moves/1, apply_moves/2]).

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

-spec test(model(), any(), dimension(), dimension(),
           pos_integer(), pos_integer()) -> ok.
test(Model, Seed, Width, Height, NAnts, Steps) ->
    io:format("ListBased:~n"),
    random:seed(Seed),
    start(Model, list_based, Width, Height, NAnts, Steps, true),
    io:format("Gb_treeBased:~n"),
    random:seed(Seed),
    start(Model, gbtree_based, Width, Height, NAnts, Steps, true).

-spec start(model(), world_impl(), dimension(), dimension(),
            pos_integer()) -> ok.
start(Model, Impl, Width, Height, Steps) ->
    start(Model, Impl, Width, Height, 1, Steps, true).

-spec start(model(), world_impl(), dimension(), dimension(),
            pos_integer(), pos_integer(), boolean()) -> ok.
start(Model, Impl, Width, Height, PopulationSize, Steps, Log) ->
    Board = create_world(Impl, Width, Height),
    Ants = create_ants(Impl, PopulationSize, Width, Height),
    Env = #env{agents = Ants, world = Board, backend = Impl},

    Animate = true,

    logger:start(Model, Env, Log, Animate),
    T1 = erlang:now(),

    EndEnv = Model:run(Steps, Env),

    T2 = erlang:now(),
    logger:stop(EndEnv),

    Time = timer:now_diff(T2, T1),
    TimeInSecs = Time / 1000000,
    io:format("Time elapsed: ~p. Time per iteration: ~p s~n",
              [TimeInSecs, TimeInSecs / Steps]).


-spec update_cell(cell()) -> cell().
update_cell({dead}) -> {alive};
update_cell({alive}) -> {dead}.

-spec get_cell(world_impl(), position(), world()) -> cell().
get_cell(Impl, {X, Y}, World) ->
    world_impl:get_cell(Impl, {X, Y}, World).

-spec get_moves(environment()) -> [{Old :: ant(), New :: ant()}].
get_moves(E = #env{agents = Agents}) ->
    GetMove = fun (A) ->
                      New = move_agent(A, E),
                      {A#ant{dir = New#ant.dir}, New}
              end,
    lists:map(GetMove, Agents).

-spec apply_moves([{ant(), ant()}], environment()) ->
                         {[ant()], environment()}.
apply_moves(Moves, Env) ->
    ApplyMove = fun (Move, E) -> apply_move(Move, E) end,
    lists:foldl(ApplyMove, Env, Moves).

% internal functions

-spec create_ants(pos_integer(), dimension(), dimension()) -> [ant()].
create_ants(PopulationSize, Width, Height) ->
    ShuffledCellPositions = util:shuffle(util:all_positions(Width, Height)),
    AntPositions = lists:sublist(ShuffledCellPositions, 1, PopulationSize),
    [#ant{pos = Pos, dir = util:random_direction()} || Pos <- AntPositions].

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

create_world(Impl, W, H)->
    Board = world_impl:create_board(Impl, W, H),
    #world{board = Board, w = W, h = H}.

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

-spec apply_move({ant(), ant()}, environment()) -> environment().
apply_move({Old, New}, E) ->
    IsPosTaken = fun(#ant{pos = P}) -> P == New#ant.pos end,
    case lists:any(IsPosTaken, E#env.agents) of
        true ->
            E;
        false ->
            NewAgents = [A || A <- E#env.agents, A#ant.pos /= Old#ant.pos],
            update_cell(Old#ant.pos, E#env{agents = [New | NewAgents]})
    end.

-spec update_cell(position(), environment()) -> environment().
update_cell(Pos, E = #env{backend = Impl, world = World}) ->
    E#env{world = world_impl:update_cell(Impl, Pos, World)}.
