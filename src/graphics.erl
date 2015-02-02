%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jun 2014 2:33 PM
%%%-------------------------------------------------------------------
-module(graphics).
-author("piotr").

-include("parallant.hrl").

-export([print/4]).


-spec print(environment(),
            Width :: dimension(),
            Height :: dimension(),
            config()) -> ok.
print(Env, Width, Height, Config) ->
    PrintCell = fun ({I, J, K}) ->
                        Agent = agents:get_agent({I, J, K}, Env, Config),
                        io:format("~c ", [model:get_agent_char(Agent, Config)])
                end,
    K = 1,
    [begin
         [PrintCell({I, J, K}) || I <- lists:seq(1, Width)],
         io:format("~n")
     end || J <- lists:reverse(lists:seq(1, Height))],
    io:format("~n").
