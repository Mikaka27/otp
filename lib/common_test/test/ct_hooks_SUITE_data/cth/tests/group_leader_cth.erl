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


-module(group_leader_cth).


-include_lib("common_test/src/ct_util.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(COMMENT,
    ct:comment(io_lib:format("~p", ?FUNCTION_NAME))
).
-define(COMMENT(A1),
    ct:comment(io_lib:format("~p(~p)", [?FUNCTION_NAME, A1]))
).
-define(COMMENT(A1, A2),
    ct:comment(io_lib:format("~p(~p, ~p)", [?FUNCTION_NAME, A1, A2]))
).
-define(COMMENT(A1, A2, A3),
    ct:comment(io_lib:format("~p(~p, ~p, ~p)", [?FUNCTION_NAME, A1, A2, A3]))
).
-define(COMMENT(A1, A2, A3, A4),
    ct:comment(io_lib:format("~p(~p, ~p, ~p, ~p)", [?FUNCTION_NAME, A1, A2, A3, A4]))
).
-define(COMMENT(A1, A2, A3, A4, A5),
    ct:comment(io_lib:format("~p(~p, ~p, ~p, ~p, ~p)", [?FUNCTION_NAME, A1, A2, A3, A4, A5]))
).

%% CT Hooks
-compile(export_all).

id(Opts) ->
    % ?COMMENT(Opts),
    empty_cth:id(Opts).

init(Id, Opts) ->
    ?COMMENT(Id, Opts),
    State = empty_cth:init(Id, Opts),
    Result = group_leader() =:= test_server_io:get_gl(true),
    gen_event:notify(?CT_EVMGR_REF, #event{ name = cth, node = node(),
                                            data = {?MODULE, init, [Id, Opts, Result]}}),
    {ok, State}.

pre_init_per_suite(Suite, Config, State) ->
    % ?COMMENT(Suite, Config, State),
    empty_cth:pre_init_per_suite(Suite,Config,State).

post_init_per_suite(Suite, Config, Return, State) ->
    % ?COMMENT(Suite, Config, Return, State),
    empty_cth:post_init_per_suite(Suite, Config, Return, State).

pre_end_per_suite(Suite, Config, State) ->
    % ?COMMENT(Suite, Config, State),
    empty_cth:pre_end_per_suite(Suite, Config, State).

post_end_per_suite(Suite, Config, Return, State) ->
    % ?COMMENT(Suite, Config, Return, State),
    empty_cth:post_end_per_suite(Suite, Config, Return, State).

pre_init_per_group(Suite, Group, Config, State) ->
    % ?COMMENT(Suite, Group, Config, State),
    empty_cth:pre_init_per_group(Suite, Group, Config, State).

post_init_per_group(Suite, Group, Config, Return, State) ->
    ?COMMENT(Suite, Group, Config, Return, State),
    empty_cth:post_init_per_group(Suite, Group, Config, Return, State).

pre_end_per_group(Suite, Group, Config, State) ->
    empty_cth:pre_end_per_group(Suite, Group, Config, State).

post_end_per_group(Suite, Group, Config, Return, State) ->
    empty_cth:post_end_per_group(Suite, Group, Config, Return, State).

pre_init_per_testcase(Suite, TC, Config, State) ->
    empty_cth:pre_init_per_testcase(Suite, TC, Config, State).

post_init_per_testcase(Suite, TC, Config, Return, State) ->
    empty_cth:post_init_per_testcase(Suite, TC, Config, Return, State).

pre_end_per_testcase(Suite, TC, Config, State) ->
    empty_cth:pre_end_per_testcase(Suite, TC, Config, State).

post_end_per_testcase(Suite, TC, Config, Return, State) ->
    empty_cth:post_end_per_testcase(Suite, TC, Config, Return, State).

on_tc_fail(Suite, TC, Reason, State) ->
    empty_cth:on_tc_fail(Suite, TC, Reason, State).

on_tc_skip(Suite, TC, Reason, State) ->
    empty_cth:on_tc_skip(Suite, TC, Reason, State).

terminate(State) ->
    empty_cth:terminate(State).

% loop() ->
%     receive
%         {From, {comment, Func}} ->
%             ct:comment(io_lib:format("~p", [Func])),
%             From ! ok,
%             loop();
%         {From, {comment, Func, A1}} ->
%             ct:comment(io_lib:format("~p(~p)", [Func, A1])),
%             From ! ok,
%             loop();
%         {From, {comment, Func, A1, A2}} ->
%             ct:comment(io_lib:format("~p(~p, ~p)", [Func, A1, A2])),
%             From ! ok,
%             loop();
%         {From, {comment, Func, A1, A2, A3}} ->
%             ct:comment(io_lib:format("~p(~p, ~p, ~p)", [Func, A1, A2, A3])),
%             From ! ok,
%             loop();
%         {From, {comment, Func, A1, A2, A3, A4}} ->
%             ct:comment(io_lib:format("~p(~p, ~p, ~p, ~p)", [Func, A1, A2, A3, A4])),
%             From ! ok,
%             loop();
%         {From, {comment, Func, A1, A2, A3, A4, A5}} ->
%             ct:comment(io_lib:format("~p(~p, ~p, ~p, ~p, ~p)", [Func, A1, A2, A3, A4, A5])),
%             From ! ok,
%             loop();
%         stop ->
%             ok;
%         _Other ->
%             loop()
%     end.
