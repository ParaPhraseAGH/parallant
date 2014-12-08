-module(model_forams).
-behaviour(model).

-include("parallant.hrl").

-export([initial_population/4,
         move/3,
         get_agent_char/2]).

-type algae_level() :: non_neg_integer().
-type foram_agent_state() :: {algae_level()}.
-type ant_state() :: parallant:ant_state(foram_agent_state()).

-spec initial_population(PopulationSize :: pos_integer(),
                         Width :: dimension(),
                         Height :: dimension(),
                         Config :: config()) ->
                                [{position(), ant_state()}].
initial_population(PopulationSize, Width, Height, _Config) ->
    AllPositions = [{I, J} || I <- lists:seq(1, Width),
                              J <- lists:seq(1, Height)],
    AlgaePositions = lists:sublist(shuffle(AllPositions),
                                   1,
                                   PopulationSize * 1),
    InitialAlgaeLevel = 3,
    [{Pos, {InitialAlgaeLevel}} || Pos <- AlgaePositions].


-spec shuffle(list()) -> list().
shuffle(L) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- L])].


-spec move(position(), environment(), config()) -> environment().
move(Pos, E, Config) ->
    %% based on agent state and its neighbourhood
    %% compute the new agent state and neighbourhood
    %% algae dispersion
    #env{world = #world{w = W, h = H}} = E,
    UpdateNeighbour =
        fun(NPos, Env) ->
                Neighbour = ants_impl:get_agent(NPos, Env, Config),
                Level = case Neighbour of
                            empty -> 0;
                            {PreviousLevel} -> PreviousLevel
                        end,
                ants_impl:update_agent(NPos, {Level + 1}, Env, Config)
        end,
    State = ants_impl:get_agent(Pos, E, Config),
    NewEnv = case State of
                 empty ->
                     E;
                 {Level} when Level =< 0 orelse Level >= 10 ->
                     E;
                 {Level} when Level > 4 ->
                     Ns = neighbours_4(Pos, W, H),
                     E1 = lists:foldl(UpdateNeighbour, E, Ns),
                     NewLevel = Level - length(Ns),
                     ants_impl:update_agent(Pos, {NewLevel}, E1, Config);
                 {Level} ->
                     ants_impl:update_agent(Pos, {Level+1}, E, Config)
             end,
    NewEnv.

-spec neighbours_4(position(), dimension(), dimension()) -> [position()].
neighbours_4({X, Y}, W, H) ->
    [{torus_bounds(X+1, W), Y},
     {torus_bounds(X-1, W), Y},
     {X, torus_bounds(Y+1, H)},
     {X, torus_bounds(Y-1, H)}].

-spec torus_bounds(dimension(), dimension()) -> dimension().
torus_bounds(Val, Max) when Val < 1 -> Max + Val;
torus_bounds(Val, Max) when Val > Max -> Val - Max;
torus_bounds(Val, _Max) -> Val.

%% displaying agents

-spec get_agent_char(ant_state(), config()) -> char().
get_agent_char(empty, _Config) ->
    $.;
get_agent_char({Level}, _Config) ->
    algae_char(Level).

-spec algae_char(algae_level()) -> char().
algae_char(Level) when Level > 9 ->
    $#;
algae_char(Level) ->
    Level + $0.
