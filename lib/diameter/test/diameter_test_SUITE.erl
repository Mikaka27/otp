%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2013-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%
%% Tests of application_opt() request_errors. There's some overlap
%% between this suite and the traffic suite but latter exercises more
%% config.
%%

-module(diameter_test_SUITE).

%% common_test wrapping
-export([
         %% Framework functions
         suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,
         
         %% The test cases
         test1/1,
         test2/1,
         test3/1
        ]).

-include("diameter_util.hrl").


%% ===========================================================================

-define(XL(F),    ?XL(F, [])).
-define(XL(F, A), ?LOG("D3XS", F, A)).


%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 90}}].

all() ->
    [{group, single}].
    % [{group, normal},
    %  {group, shuffle},
    %  {group, parallel},
    %  {group, sequence}].

groups() ->
    [{single, [], [test1]},
     {normal, [], [test1, test2, test3]},
     {shuffle, [shuffle], [test1, test2, test3]},
     {parallel, [parallel], [test1, test2, test3]},
     {sequence, [sequence], [test1, test2, test3]}].

init_per_suite(Config) ->
    ?XL("init_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    % Now = fun() ->
    %     {MS,S,US} = os:timestamp(),
    %     {{Year,Month,Day}, {Hour,Min,Sec}} = calendar:now_to_local_time({MS,S,US}),
    %     MilliSec = trunc(US/1000),
    %     lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B "
    %                             "~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
    %                             [Year,Month,Day,Hour,Min,Sec,MilliSec]))
    % end,
    % WriteToFile = fun(Format) ->
    %     file:write_file("/home/emiwaso/Projects/otp/out.txt", Format, [append])
    % end,
    % Tracer = spawn(fun F() ->
    %     try
    %         receive
    %             {trace, Pid, call, {erlang, group_leader, [GL, Target]}} = M ->
    %                 PidInfo = process_info(Pid),
    %                 GLInfo = process_info(GL),
    %                 TargetInfo = process_info(Target),
    %                 {group_leader, TargetGL} = process_info(Target, group_leader),
    %                 TargetGLInfo = process_info(TargetGL),
    %                 WriteToFile(io_lib:fwrite("~n~nNow: ~p~nMessage: ~p~nPidInfo: ~p~nGLInfo: ~p~nTargetInfo: ~p~nTargetGLInfo: ~p~nStatus: ~p~n", [Now(), M, PidInfo, GLInfo, TargetInfo, TargetGLInfo, ct:get_status()]));
    %             {trace, Pid, call, _} = M ->
    %                 PidInfo = process_info(Pid),
    %                 {group_leader, GL} = process_info(Pid, group_leader),
    %                 GLInfo = process_info(GL),
    %                 WriteToFile(io_lib:fwrite("~n~nNow: ~p~nMessage: ~p~nPidInfo: ~p~nGLInfo: ~p~nStatus: ~p~n", [Now(), M, PidInfo, GLInfo, ct:get_status()]));
    %             {trace, Pid, send, {comment, _Comment}, To} = M ->
    %                 PidInfo = process_info(Pid),
    %                 ToInfo = process_info(To),
    %                 {group_leader, ToGL} = process_info(To, group_leader),
    %                 ToGLInfo = process_info(ToGL),
    %                 WriteToFile(io_lib:fwrite("~n~nNow: ~p~nMessage: ~p~nPidInfo: ~p~nToInfo: ~p~nToGLInfo: ~p~nStatus: ~p~n", [Now(), M, PidInfo, ToInfo, ToGLInfo, ct:get_status()]));
    %             {trace, Pid, 'receive', {comment, _Comment}} = M ->
    %                 PidInfo = process_info(Pid),
    %                 {group_leader, GL} = process_info(Pid, group_leader),
    %                 GLInfo = process_info(GL),
    %                 WriteToFile(io_lib:fwrite("~n~nNow: ~p~nMessage: ~p~nPidInfo: ~p~nGLInfo: ~p~nStatus: ~p~n", [Now(), M, PidInfo, GLInfo, ct:get_status()]));
    %             _Other ->
    %                 ok
    %         end
    %     catch C : R : ST ->
    %         WriteToFile(io_lib:fwrite("~n~nNow: ~p~nEXCEPTION: ~p:~p:~p~n", [Now(), C, R, ST]))
    %     end,
    %     F()
    % end),
    % Session = trace:session_create(test, Tracer, []),
    % trace:process(Session, all, true, [call, send, 'receive']),
    % trace:function(Session, {erlang, group_leader, 2}, true, [local]),
    % trace:function(Session, {tes_cth, init, 2}, true, [local]),
    % trace:function(Session, {tes_cth, call_init_tc, 3}, true, [local]),
    % trace:function(Session, {tes_cth, call_end_tc, 4}, true, [local]),
    % trace:function(Session, {tes_cth, init_testcase, 2}, true, [local]),
    % trace:function(Session, {tes_cth, end_testcase, 4}, true, [local]),
    % trace:function(Session, {tes_rcs, start, 2}, true, [local]),
    % trace:function(Session, {tes_rcs, init, 2}, true, [local]),
    % trace:function(Session, {tes_rcs_lib, do_node_seq, 5}, true, [local]),
    % trace:function(Session, {tes_rcs_lib, node_cmd, 5}, true, [local]),
    % trace:send(Session, true, []),
    % trace:recv(Session, true, []),
    % trace:function(Session, {node_controller, init, 0}, true, []),
    % trace:function(Session, {node_controller, comment, 2}, true, [local]),
    example_group_leader:start(gl1, group_leader()),
    % ?DUTIL:init_per_suite([{session, Session}, {tracer, Tracer} | Config]).
    ?DUTIL:init_per_suite(Config).

end_per_suite(Config) ->
    ?XL("end_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    % trace:session_destroy(proplists:get_value(session, Config)),
    % exit(proplists:get_value(tracer, Config), kill),
    ?DUTIL:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    ?XL("init_per_group(~p) -> entry with"
        "~n   Config: ~p", [_GroupName, Config]),
    Config.

end_per_group(_GroupName, Config) ->
    ?XL("end_per_group(~p) -> entry", [_GroupName]),
    Config.


%% This test case can take a *long* time, so if the machine is too slow, skip
init_per_testcase(Case, Config) when is_list(Config) ->
    ?XL("init_per_testcase(~w) -> entry with"
        "~n   Config: ~p"
        "~n   => check factor", [Case, Config]),
    Key = dia_factor,
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Factor}} when (Factor > 10) ->
            ?XL("init_per_testcase(~w) -> Too slow (~w) => SKIP",
                [Case, Factor]),
            {skip, {machine_too_slow, Factor}};
        _ ->
            ?XL("init_per_testcase(~w) -> run test", [Case]),
            Config
    end;
init_per_testcase(Case, Config) ->
    ?XL("init_per_testcase(~w) -> entry", [Case]),
    Config.


end_per_testcase(Case, Config) when is_list(Config) ->
    ?XL("end_per_testcase(~w) -> entry", [Case]),
    Config.

test1(_Config) ->
    % observer:start(),
    {group_leader, GL1} = process_info(whereis(node_controller), group_leader),
    {group_leader, GL2} = process_info(whereis(node_controller_child), group_leader),
    ct:pal("self GL: ~p, GL1: ~p, GL2: ~p~n", [group_leader(), GL1, GL2]),
    group_leader(whereis(gl1), whereis(node_controller_child)),
    {group_leader, GL3} = process_info(whereis(node_controller), group_leader),
    {group_leader, GL4} = process_info(whereis(node_controller_child), group_leader),
    ct:pal("self GL: ~p, GL3: ~p, GL4: ~p~n", [group_leader(), GL3, GL4]),
    timer:sleep(3000).

test2(_Config) ->
    timer:sleep(2000).

test3(_Config) ->
    timer:sleep(500).

