%%%-------------------------------------------------------------------
%%% @author Daniel Grzonka
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% Agents gb_trees based approach
%%% @end
%%% Created : 07. lis 2014 15:56
%%%-------------------------------------------------------------------
-module(agents_gbtree).
-behaviour(agents).
-author("Daniel").

%% API
-export([create_agents/3,
         get_agent/3,
         update_agent/4,
         get_positions/2,
         group_by/1,
         get_list/1,
         get_tile/2,
         update_tiles/3]).

-include("parallant.hrl").

-type tile() :: agents:tile({Start :: dimension(), End :: dimension()}).
-type agent_state() :: parallant:agent_state().
-type agents() :: agents:agents(gb_trees:tree(position(), agent_state())).
-type range() :: {position(), position()}.

-spec create_agents(PopulationSize :: pos_integer(),
                    World :: world(),
                    config()) ->
                           Agents :: agents().
create_agents(PopulationSize, World, Config) ->
    Pop = model:initial_population(PopulationSize, World, Config),
    IndividualsWithKeys = [{Pos,
                            #agent{pos = Pos, state = State}}
                           || {Pos, State} <- Pop],
    gb_trees:from_orddict(lists:sort(IndividualsWithKeys)).

-spec get_agent(position(), environment(), config()) -> agent_state().
get_agent(Position, Env, _Config) ->
    TreeRes = gb_trees:lookup(Position,  Env#env.agents),
    case TreeRes of
        {value, #agent{state = State}} ->
            State;
        none ->
            empty
    end.

-spec update_agent(position(), agent_state(), environment(), config()) ->
                          environment().
update_agent(Position, empty, Env, _Config) ->
    Env#env{agents = gb_trees:delete_any(Position, Env#env.agents)};
update_agent(Position, NewState, Env, Config) ->
    NewAgent = #agent{pos = Position, state = NewState},
    case get_agent(Position, Env, Config) of
        empty ->
            NewAgents = gb_trees:insert(Position, NewAgent, Env#env.agents),
            Env#env{agents = NewAgents};
        _ ->
            NewAgents = gb_trees:update(Position, NewAgent, Env#env.agents),
            Env#env{agents = NewAgents}
    end.

-spec get_positions(agents_lists:agents(), tile()) -> [position()].
get_positions(Agents, _Tile) ->
    [A#agent.pos || A <- Agents].

-spec group_by([{term(), [term()]}]) -> [{term(), [term()]}].
group_by(List) ->
    dict:to_list(
      lists:foldl(fun({K, V}, D) ->
                          dict:append_list(K, V, D)
                  end, dict:new(), List)).

-spec get_list(agents()) ->
                      [agent()].
get_list(Agents) ->
    gb_trees:values(Agents).

-spec get_tile(range(), environment()) ->
                      {range(), agents()}.
get_tile(Range, Env) ->
    {{X1, Y1, Z1}, {X2, Y2, Z2}} = Range,
    Positions = [{X,Y,Z} ||
                    X <- lists:seq(X1,X2),
                    Y <- lists:seq(Y1,Y2),
                    Z <- lists:seq(Z1,Z2),
                    X =< (Env#env.world)#world.w, Y =< (Env#env.world)#world.h, Z =< (Env#env.world)#world.d],
    GetAgent = fun(Position) ->
                       TreeRes = gb_trees:lookup(Position,  Env#env.agents),
                       case TreeRes of
                           {value, A} ->
                               A;
                           none ->
                               empty
                       end
               end,
    Agents = [A || A<- lists:map(GetAgent, Positions), A /= empty],
    {Range, Agents}.

-spec update_tiles([environment()], environment(), config()) -> environment().
update_tiles(NewEnvs, Env, Config) ->
    ApplyEnv = mk_apply_env(Config),
    lists:foldl(ApplyEnv, Env, NewEnvs).

mk_apply_env(Config) ->
    fun({Tile, TileEnv}, EAcc) ->
            UpdateAgent =
                fun(Pos, EAcc2) ->
                        NewState = get_agent(Pos, TileEnv, Config),
                        update_agent(Pos, NewState, EAcc2, Config)
                end,
            Neighbours = agents:neighbourhood(Tile, TileEnv, Config),
            lists:foldl(UpdateAgent, EAcc, Neighbours)
    end.
