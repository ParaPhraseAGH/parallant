-module(model_forams).
-behaviour(model).

-include("parallant.hrl").

-export([initial_population/3,
         move/3,
         log_custom/3,
         get_agent_char/2]).

-type algae_level() :: non_neg_integer().
-type foram_agent_state() :: {algae_level()}.
-type agent_state() :: parallant:agent_state(foram_agent_state()).

-spec initial_population(PopulationSize :: pos_integer(),
                         World :: world(),
                         Config :: config()) ->
                                [{position(), agent_state()}].
initial_population(PopulationSize, World, _Config) ->
    #world{w = Width, h = Height, d = _Depth} = World,
    K = 1,
    AllPositions = [{I, J, K} || I <- lists:seq(1, Width),
                                 J <- lists:seq(1, Height)],
    %% K <- lists:seq(1, Height)],
    AlgaePositions = lists:sublist(algorithm:shuffle(AllPositions),
                                   1,
                                   PopulationSize * 1),
    InitialAlgaeLevel = 3,
    [{Pos, {InitialAlgaeLevel}} || Pos <- AlgaePositions].


-spec move(position(), environment(), config()) -> environment().
move(Pos, E, Config) ->
    %% pretend we have some hevy computing here
    work(Config),
    %% based on agent state and its neighbourhood
    %% compute the new agent state and neighbourhood
    %% algae dispersion
    UpdateNeighbour =
        fun(NPos, Env) ->
                Neighbour = agents:get_agent(NPos, Env, Config),
                Level = case Neighbour of
                            empty -> 0;
                            {PreviousLevel} -> PreviousLevel
                        end,
                agents:update_agent(NPos, {Level + 1}, Env, Config)
        end,
    State = agents:get_agent(Pos, E, Config),
    NewEnv = case State of
                 empty ->
                     E;
                 {Level} when Level =< 0 orelse Level >= 10 ->
                     E;
                 {Level} when Level > 4 ->
                     Ns = neighbours_4(Pos, E#env.world),
                     E1 = lists:foldl(UpdateNeighbour, E, Ns),
                     NewLevel = Level - length(Ns),
                     agents:update_agent(Pos, {NewLevel}, E1, Config);
                 {Level} ->
                     agents:update_agent(Pos, {Level+1}, E, Config)
             end,
    NewEnv.

%% TODO update neighbours with depth
-spec neighbours_4(position(), world()) -> [position()].
neighbours_4({X, Y, Z}, #world{w = W, h = H, d = _D}) ->
    [{torus_bounds(X+1, W), Y, Z},
     {torus_bounds(X-1, W), Y, Z},
     {X, torus_bounds(Y+1, H), Z},
     {X, torus_bounds(Y-1, H), Z}].

-spec torus_bounds(dimension(), dimension()) -> dimension().
torus_bounds(Val, Max) when Val < 1 -> Max + Val;
torus_bounds(Val, Max) when Val > Max -> Val - Max;
torus_bounds(Val, _Max) -> Val.

work(_Config = #config{}) ->
    work(10);
work(N) ->
    %% based on Rastrigin function
    S = [random:uniform() || _ <- lists:seq(1,N)],
    lists:foldl(fun(X, Sum) -> Sum + 10 + X*X - 10*math:cos(2*math:pi()*X) end, 0.0, S).

%% displaying agents

-spec get_agent_char(agent_state(), config()) -> char().
get_agent_char(empty, _Config) ->
    $.;
get_agent_char({Level}, _Config) ->
    algae_char(Level).

-spec algae_char(algae_level()) -> char().
algae_char(Level) when Level > 9 ->
    $#;
algae_char(Level) ->
    Level + $0.

%% logging
-spec log_custom(Step :: pos_integer(), environment(), config()) -> ok.
log_custom(_Step, _Env, _Config) ->
    %% cast log_custom to logger
    ok.
