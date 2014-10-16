-module(logger).
-export([start/2, stop/3, log/3]).
-include("parallant.hrl").

-define(LOG_DELAY, 50). % ms
-define(MAX_WIDTH_TO_SHOW, 65).

-ifdef(dont_overwrite_disp).
overwrite_display(_) -> ok.
-else.

overwrite_display(Height) ->
    io:format("\033[~pA", [Height + 2]). % display in the same place as the previous step

-endif.

-ifdef(debug).

-spec log(model(), environment(), pos_integer()) -> ok.
log(Model, Env, Step) ->
    io:format("Step ~p:~n", [Step + 1]),
    Model:display(Env),
    timer:sleep(?LOG_DELAY),
    #env{world = #world{h = Height}} = Env,
    overwrite_display(Height).

-else.

-spec log(any(), any(), any()) -> ok.
log(_, _, _) ->  ok.
-endif.


start(Model, E = #env{world = W}) ->
    #world{w = Width} = W,
    if
        Width < ?MAX_WIDTH_TO_SHOW ->
            io:format("Ants: ~p~n", [E#env.agents]),
            io:format("Step 1:~n"),
            Model:display(E);
        true -> ok
    end.

stop(Model, E = #env{world = W}, Steps) ->
    #world{w = Width} = W,
    if
        Width < ?MAX_WIDTH_TO_SHOW ->
            io:format("Step ~p:~n", [Steps]),
            Model:display(E);
        true -> ok
    end.
