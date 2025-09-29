-module(example_group_leader).

-export([start/2, get_parent_gl/1]).

start(Name, ParentGL) ->
    GL = spawn(fun() -> init(Name, ParentGL) end),
    GL.

init(Name, ParentGL) ->
    register(Name, self()),
    loop(Name, ParentGL).

get_parent_gl(Name) ->
    Name ! {self(), get_parent_gl},
    receive
        {parent_gl, ParentGL} -> ParentGL
    after 5000 ->
        undefined
    end.

loop(Name, ParentGL) ->
    receive
        {From, get_parent_gl} ->
            From ! {parent_gl, ParentGL},
            loop(Name, ParentGL);
        Msg ->
            file:write_file("/mnt/D/Projects/otp/out.txt", io_lib:fwrite("~p: ~p~n", [Name, Msg]), [append]),
            ParentGL ! Msg,
            loop(Name, ParentGL)
    end.