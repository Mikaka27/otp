%%% Common Test Example Common Test Hook module.
%%%
%%% To use this hook, on the command line:
%%%     ct_run -suite example_SUITE -pa . -ct_hooks example_cth
%%%
%%% Note `-pa .`: the hook beam file must be in the code path when installing.
-module(example_cth).

%% Mandatory Callbacks
-export([init/2]).

-export([pre_init_per_suite/3, post_init_per_suite/4]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3, post_init_per_group/4]).
-export([pre_end_per_group/4, post_end_per_group/5]).

-export([pre_init_per_testcase/3]).
-export([post_init_per_testcase/3]).
-export([post_end_per_testcase/4]).

% -export([on_tc_skip/4]).

-export([terminate/1]).

%% This hook state is threaded through all the callbacks.
-record(state, {node_controller :: pid()}).

%% Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, _Opts) ->
    Now = fun() ->
        {MS,S,US} = os:timestamp(),
        {{Year,Month,Day}, {Hour,Min,Sec}} = calendar:now_to_local_time({MS,S,US}),
        MilliSec = trunc(US/1000),
        lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B "
                                "~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
                                [Year,Month,Day,Hour,Min,Sec,MilliSec]))
    end,
    WriteToFile = fun(Format) ->
        file:write_file("/mnt/D/Projects/otp/out.txt", Format, [append])
    end,
    Tracer = spawn(fun F() ->
        try
            receive
                {trace, Pid, call, {erlang, group_leader, [GL, Target]}} = M ->
                    PidInfo = process_info(Pid),
                    GLInfo = process_info(GL),
                    TargetInfo = process_info(Target),
                    TargetGLInfo = case process_info(Target, group_leader) of
                        {group_leader, TargetGL} ->
                            process_info(TargetGL);
                        undefined ->
                            undefined
                    end,
                    WriteToFile(io_lib:fwrite("~n~nNow: ~p~nMessage: ~p~nPidInfo: ~p~nGLInfo: ~p~nTargetInfo: ~p~nTargetGLInfo: ~p~nStatus: ~p~n", [Now(), M, PidInfo, GLInfo, TargetInfo, TargetGLInfo, ct:get_status()]));
                {trace, Pid, call, _} = M ->
                    PidInfo = process_info(Pid),
                    GLInfo = case process_info(Pid, group_leader) of
                        {group_leader, GL} ->
                            process_info(GL);
                        undefined ->
                            undefined
                    end,
                    ParentInfo = case process_info(Pid, parent) of
                        {parent, Parent} ->
                            process_info(Parent);
                        undefined ->
                            undefined
                    end,
                    WriteToFile(io_lib:fwrite("~n~nNow: ~p~nMessage: ~p~nPidInfo: ~p~nParentInfo: ~p~nGLInfo: ~p~nStatus: ~p~n", [Now(), M, PidInfo, ParentInfo, GLInfo, ct:get_status()]));
                {trace, Pid, send, {comment, _Comment}, To} = M ->
                    PidInfo = process_info(Pid),
                    ToInfo = process_info(To),
                    ToGLInfo = case process_info(To, group_leader) of
                        {group_leader, ToGL} ->
                            process_info(ToGL);
                        undefined ->
                            undefined
                    end,
                    WriteToFile(io_lib:fwrite("~n~nNow: ~p~nMessage: ~p~nPidInfo: ~p~nToInfo: ~p~nToGLInfo: ~p~nStatus: ~p~n", [Now(), M, PidInfo, ToInfo, ToGLInfo, ct:get_status()]));
                {trace, Pid, 'receive', {comment, _Comment}} = M ->
                    PidInfo = process_info(Pid),
                    GLInfo = case process_info(Pid, group_leader) of
                        {group_leader, GL} ->
                            process_info(GL);
                        undefined ->
                            undefined
                    end,
                    WriteToFile(io_lib:fwrite("~n~nNow: ~p~nMessage: ~p~nPidInfo: ~p~nGLInfo: ~p~nStatus: ~p~n", [Now(), M, PidInfo, GLInfo, ct:get_status()]));
                _Other ->
                    ok
            end
        catch C : R : ST ->
            WriteToFile(io_lib:fwrite("~n~nNow: ~p~nEXCEPTION: ~p:~p:~p~n", [Now(), C, R, ST]))
        end,
        F()
    end),

    Self = self(),

    spawn(fun() ->
        Session = trace:session_create(test, Tracer, []),
        trace:process(Session, all, true, [call, send, 'receive']),
        trace:function(Session, {erlang, group_leader, 2}, true, [local]),
        % trace:function(Session, {tes_cth, init, 2}, true, [local]),
        % trace:function(Session, {tes_cth, call_init_tc, 3}, true, [local]),
        % trace:function(Session, {tes_cth, call_end_tc, 4}, true, [local]),
        % trace:function(Session, {tes_cth, init_testcase, 2}, true, [local]),
        % trace:function(Session, {tes_cth, end_testcase, 4}, true, [local]),
        % trace:function(Session, {tes_rcs, start, 2}, true, [local]),
        % trace:function(Session, {tes_rcs, init, 2}, true, [local]),
        % trace:function(Session, {tes_rcs_lib, do_node_seq, 5}, true, [local]),
        % trace:function(Session, {tes_rcs_lib, node_cmd, 5}, true, [local]),
        trace:function(Session, {node_controller, start, 0}, true, [local]),
        trace:function(Session, {node_controller, init, 0}, true, [local]),
        trace:function(Session, {node_controller, comment, 2}, true, [local]),
        trace:send(Session, true, []),
        trace:recv(Session, true, []),
        Self ! ok,
        timer:sleep(infinity)
    end),
    receive ok -> ok end,

    SelfInfo = process_info(Self),
    {parent, Parent} = process_info(Self, parent),
    ParentInfo = process_info(Parent),
    GL = group_leader(),
    GLInfo = process_info(GL),
    Args = init:get_arguments(),
    {current_stacktrace, CT} = process_info(Self, current_stacktrace),

    WriteToFile(io_lib:fwrite("~n~nNow: ~p~nMod: ~p, Func:~p~nSelf: ~p~nSelfInfo: ~p~nParent: ~p~nParentInfo: ~p~nGL: ~p~nGLInfo: ~p~nCmd: ~p~nCT: ~p~n", [Now(), ?MODULE, ?FUNCTION_NAME, Self, SelfInfo, Parent, ParentInfo, GL, GLInfo, Args, CT])),

    Pid = node_controller:start(),
    {ok, #state{node_controller = Pid}}.

%% Called before init_per_suite is called.
pre_init_per_suite(_Suite,Config,#state{node_controller = Pid} = State) ->
    comment(Pid, "pre_init_per_suite"),
    {Config, State}.

post_init_per_suite(_Suite,_Config,Return,#state{node_controller = Pid} = State) ->
    %% This is called after init_per_suite, but before the first test case.
    %% We can use this to initialize the state for the suite.
    comment(Pid, "post_init_per_suite"),
    {Return, State}.

%% Called after end_per_suite.
post_end_per_suite(_Suite,_Config,Return,#state{node_controller = Pid} = State) ->
    comment(Pid, "post_end_per_suite"),
    {Return, State}.

pre_init_per_group(_GroupName,Config,#state{node_controller = Pid} = State) ->
    comment(Pid, "pre_init_per_group"),
    {Config, State}.

post_init_per_group(_GroupName,_Config,Return,#state{node_controller = Pid} = State) ->
    comment(Pid, "post_init_per_group"),
    {Return, State}.

pre_end_per_group(_Suite, _Group, Config, #state{node_controller = Pid} = State) ->
    comment(Pid, "pre_end_per_group"),
    {Config, State}.

post_end_per_group(_Suite, _Group, _Config, Return, #state{node_controller = Pid} = State) ->
    comment(Pid, "post_end_per_group"),
    {Return, State}.

%% Called before each init_per_testcase.
pre_init_per_testcase(_TC,Config,#state{node_controller = Pid} = State) ->
    comment(Pid, "pre_init_per_testcase"),
    {Config, State}.

post_init_per_testcase(_TC,Config,#state{node_controller = Pid} = State) ->
    comment(Pid, "post_init_per_testcase"),
    {Config, State}.

%% Called after each end_per_testcase.
post_end_per_testcase(_TC,_Config,Return,#state{node_controller = Pid} = State) ->
    comment(Pid, "post_end_per_testcase"),
    {Return, State}.

%% Called when a test case is skipped by either user action
%% or due to an init function failing.
% on_tc_skip(_Suite, _TC, _Reason, #state{node_controller = Pid} = State) ->
%     State.

%% Called when the scope of the CTH is done.
terminate(#state{node_controller = Pid}) ->
    exit(Pid, kill),
    ok.

comment(NodeController, "post_end_per_testcase") ->
    Self = self(),
    spawn(fun() ->
        timer:sleep(5000),
        NodeController ! {Self, "post_end_per_testcase"}
    end),
    ok;
comment(_NodeController, _Comment) ->
    ok.

% comment(NodeController, Comment) ->
%     comment(NodeController, Comment, 10000),
%     receive_ok(10000).

% comment(_NodeController, _Comment, 0) ->
%     ok;
% comment(NodeController, Comment, Count) ->
%     Self = self(),
%     spawn(fun() -> NodeController ! {Self, Comment} end),
%     comment(NodeController, Comment, Count - 1).

% receive_ok(0) ->
%     ok;
% receive_ok(Count) ->
%     receive
%         ok -> ok
%     end,
%     receive_ok(Count - 1).