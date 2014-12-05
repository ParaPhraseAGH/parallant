-module(model_forams).
-behaviour(model).

-include("parallant.hrl").

-export([initial_population/4,
         move/3,
         get_agent_char/2]).

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


neighbours_4({X,Y}, W, H) ->
    [{torus_bounds(X+1, W), Y},
     {torus_bounds(X-1, W), Y},
     {X, torus_bounds(Y+1, H)},
     {X, torus_bounds(Y-1, H)}].


torus_bounds(Val, Max) when Val < 1 -> Max + Val;
torus_bounds(Val, Max) when Val > Max -> Val - Max;
torus_bounds(Val, _Max) -> Val.

%% displaying agents

-spec get_agent_char(ant_state(), config()) -> char().
get_agent_char(empty, _Config) ->
    $.;
get_agent_char({Level}, _Config) ->
    algae_char(Level).

algae_char(0) ->
    $.;
algae_char(Level) ->
    Level + $0.
