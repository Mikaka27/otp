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
    [{timetrap, {seconds, 90}},
     {ct_hooks, [example_cth]}].

all() ->
    [{group, single}].
    %  {group, normal},
    %  {group, shuffle},
    %  {group, parallel}].

groups() ->
    [{single, [], [test1]},
     {normal, [], [test1, test2, test3]},
     {shuffle, [shuffle], [test1, test2, test3]},
     {parallel, [parallel], [test1, test2, test3]}].

init_per_suite(Config) ->
    ?XL("init_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    Tracer = spawn(fun F() ->
        receive
            {trace, Pid, _, _} = M ->
                ct:pal("Message: ~p~nInfo: ~p~n", [M, process_info(Pid)]),
                F()
        end
    end),
    Session = trace:session_create(test, Tracer, []),
    trace:process(Session, all, true, [call]),
    trace:function(Session, {erlang, group_leader, 2}, true, []),
    ?DUTIL:init_per_suite([{session, Session}, {tracer, Tracer} | Config]).

end_per_suite(Config) ->
    ?XL("end_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    trace:session_destroy(proplists:get_value(session, Config)),
    exit(proplists:get_value(tracer, Config), kill),
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
    timer:sleep(1000).

test2(_Config) ->
    timer:sleep(2000).

test3(_Config) ->
    timer:sleep(500).

