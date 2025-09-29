%%% Node controller
-module(node_controller).

-export([start/0]).

start() ->
    % case whereis(node_controller) of
    %     undefined ->
    %         spawn(fun() -> loop() end);
    %     Pid when is_pid(Pid) ->
    %         Pid
    % end.
    spawn(fun() -> init() end).

init() ->
    print_info(),
    ct:comment("Test"),
    print_info(),
    loop().

print_info() ->
    Self = self(),
    {parent, Parent} = process_info(Self, parent),
    SelfGL = group_leader(),
    {group_leader, ParentGL} = process_info(Parent, group_leader),
    TestGL = test_server_io:get_gl(true),
    Supervisor = test_server_gl:get_tc_supervisor(TestGL),
    CtGL = whereis(ct_default_gl),
    User = whereis(user),
    {group_leader, UserGL} = process_info(User, group_leader),
    {group_leader, CtGLParentGL} = process_info(CtGL, group_leader),
    TestServerGL = whereis(test_server_gl),
    ct:pal("Self: ~p, SelfGL: ~p~n", [Self, SelfGL]),
    ct:pal("Parent: ~p, ParentGL: ~p~n", [Parent, ParentGL]),
    ct:pal("TestGL(true): ~p, Supervisor: ~p~n", [TestGL, Supervisor]),
    ct:pal("CtGL: ~p, CtGLParentGL: ~p~n", [CtGL, CtGLParentGL]),
    ct:pal("TestServerGL: ~p~n", [TestServerGL]),
    ct:pal("User: ~p, UserGL: ~p~n", [User, UserGL]).

loop() ->
    receive
        {From, Comment} ->
            ct:comment(Comment),
            From ! ok
    end,
    loop().