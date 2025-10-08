%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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

-module(ct_hooks_group_leader_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

%% Test server callback functions
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [check_group_leader].

%% Test cases starts here.
check_group_leader(Config) ->
    Cth = whereis(group_leader_cth),
    CthInfo = catch process_info(Cth),
    {group_leader, GL} = lists:keyfind(group_leader, 1, CthInfo),
    GLInfo = catch process_info(GL),
    TcGL = test_server_io:get_gl(true),
    TcGLInfo = catch process_info(TcGL),
    ct:pal("Cth: ~p~nCthInfo: ~p~nGL: ~p~nGLInfo: ~p~nTcGL: ~p~nTcGLInfo: ~p~n",
           [Cth, CthInfo, GL, GLInfo, TcGL, TcGLInfo]).
