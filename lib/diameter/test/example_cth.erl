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