%%% Node controller
-module(node_controller).

-export([start/0]).

start() ->
    case whereis(node_controller) of
        undefined ->
            spawn(fun() -> loop() end);
        Pid when is_pid(Pid) ->
            Pid
    end.

loop() ->
    receive
        {From, Comment} ->
            ct:comment(Comment),
            From ! ok
    end,
    loop().