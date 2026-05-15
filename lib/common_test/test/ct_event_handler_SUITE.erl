%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File: ct_event_handler_SUITE.erl
%%%
%%% Description: This suite will install event handlers and run
%%% some simple tests to check that events are generated according
%%% to the specification (see Event Handling in CT User's Guide).
%%%-------------------------------------------------------------------
-module(ct_event_handler_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/src/ct_util.hrl").

%-include_lib("common_test/include/ct_event.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),

    EH = filename:join(DataDir, "eh_A.erl"),
    CResult = compile:file(EH, [verbose,report,{outdir,PrivDir}]),
    test_server:format("~s compilation result: ~p~n", [EH,CResult]),

    Config1 = ct_test_support:init_per_suite(Config, 0),
    Config1.

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start_stop, results, nested_groups, multiple_cases,
     group_path, event_mgrs, test_run_start_event].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
	Config.



%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

start_stop(doc) -> 
    [];

start_stop(suite) -> 
    [];

start_stop(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    TestObj = filename:join(DataDir, "event_handling_1"),
    Suite1 = filename:join(TestObj, "test/eh_11_SUITE"),
    Opts0 = ct_test_support:get_opts(Config),

    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],

    Opts = Opts0 ++ [{suite,Suite1},{testcase,tc1},
		     {event_handler,{eh_A,EvHArgs}}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),
    
    Events = ct_test_support:get_events(ERPid, Config),    

    ct_test_support:log_events(start_stop, 
			       ct_test_support:reformat(Events, eh_A),
			       ?config(priv_dir, Config),
			       Opts),
    
    TestEvents =
	[{eh_A,start_logging,{'DEF','RUNDIR'}},
	 {eh_A,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
	 {eh_A,start_info,{1,1,1}},
	 {eh_A,tc_start,{eh_11_SUITE,init_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,init_per_suite,ok}},
	 {eh_A,tc_start,{eh_11_SUITE,tc1}},
	 {eh_A,tc_done,{eh_11_SUITE,tc1,ok}},
	 {eh_A,test_stats,{1,0,{0,0}}},
	 {eh_A,tc_start,{eh_11_SUITE,end_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,end_per_suite,ok}},
	 {eh_A,test_done,{'DEF','STOP_TIME'}},
	 {eh_A,stop_logging,[]}],

    ok = ct_test_support:verify_events(TestEvents++TestEvents, Events, Config).


results(doc) -> 
    [];

results(suite) -> 
    [];

results(Config) when is_list(Config) -> 
    DataDir = ?config(data_dir, Config),

    TestObj = filename:join(DataDir, "event_handling_1"),
    Suite1 = filename:join(TestObj, "test/eh_11_SUITE"),
    Opts0 = ct_test_support:get_opts(Config),

    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],

    Opts = Opts0 ++ [{suite,Suite1},
		     {event_handler,{eh_A,EvHArgs}}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),
    
    Events = ct_test_support:get_events(ERPid, Config),
    
    ct_test_support:log_events(results, 
			       ct_test_support:reformat(Events, eh_A),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents =
	[{eh_A,start_logging,{'DEF','RUNDIR'}},
	 {eh_A,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
	 {eh_A,start_info,{1,1,5}},
	 {eh_A,tc_start,{eh_11_SUITE,init_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,init_per_suite,ok}},
	 [{eh_A,tc_start,{eh_11_SUITE,{init_per_group,g1,[]}}},
	  {eh_A,tc_done,{eh_11_SUITE,{init_per_group,g1,[]},ok}},
	  {eh_A,tc_start,{eh_11_SUITE,tc1}},
	  {eh_A,tc_done,{eh_11_SUITE,tc1,ok}},
	  {eh_A,test_stats,{1,0,{0,0}}},
	  {eh_A,tc_start,{eh_11_SUITE,tc2}},
	  {eh_A,tc_done,{eh_11_SUITE,tc2,ok}},
	  {eh_A,test_stats,{2,0,{0,0}}},
	  {eh_A,tc_start,{eh_11_SUITE,tc3}},
	  {eh_A,tc_done,{eh_11_SUITE,tc3,{skipped,"Skip"}}},
	  {eh_A,test_stats,{2,0,{1,0}}},
	  {eh_A,tc_start,{eh_11_SUITE,tc4}},
	  {eh_A,tc_done,{eh_11_SUITE,tc4,{skipped,"Skipped"}}},
	  {eh_A,test_stats,{2,0,{2,0}}},
	  {eh_A,tc_start,{eh_11_SUITE,tc5}},
	  {eh_A,tc_done,{eh_11_SUITE,tc5,{failed,{error,'Failing'}}}},
	  {eh_A,test_stats,{2,1,{2,0}}},
	  {eh_A,tc_start,{eh_11_SUITE,{end_per_group,g1,[]}}},
	  {eh_A,tc_done,{eh_11_SUITE,{end_per_group,g1,[]},ok}}],
	 {eh_A,tc_start,{eh_11_SUITE,end_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,end_per_suite,ok}},
	 {eh_A,test_done,{'DEF','STOP_TIME'}},
	 {eh_A,stop_logging,[]}],

    ok = ct_test_support:verify_events(TestEvents++TestEvents, Events, Config).


event_mgrs(_) ->
    ?CT_EVMGR_REF = ct:get_event_mgr_ref(),
    ?CT_MEVMGR_REF = ct_master:get_event_mgr_ref().

test_run_start_event(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestObj = filename:join(DataDir, "event_handling_1"),
    Suite1 = filename:join(TestObj, "test/eh_11_SUITE"),
    Opts0 = ct_test_support:get_opts(Config),

    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],

    %% Run with group selection — should produce a spec_name with {group,...}
    Opts = Opts0 ++ [{suite,Suite1},{group,g1},
		     {event_handler,{eh_A,EvHArgs}}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(test_run_start_event,
			       ct_test_support:reformat(Events, eh_A),
			       ?config(priv_dir, Config),
			       Opts),

    %% Verify that a test_run_start event was emitted with spec_name
    %% containing {group,g1}
    MatchFun = fun(Data) when is_map(Data) ->
		       #{spec_name := SN} = Data,
		       case string:find(SN, "{group,g1}") of
			   nomatch -> error({bad_spec_name, SN});
			   _ -> match
		       end
	       end,
    TestEvents =
	[{eh_A,start_logging,'_'},
	 {eh_A,test_run_start,MatchFun}],

    ok = ct_test_support:verify_events(TestEvents, Events, Config).


multiple_cases(doc) ->
    "Demonstrates that running individual test cases as separate jobs "
    "produces identical init_per_suite/end_per_suite pairs that cannot "
    "be attributed to a specific job without test_run_start.";

multiple_cases(suite) ->
    [];

multiple_cases(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestObj = filename:join(DataDir, "event_handling_1"),
    Suite1 = filename:join(TestObj, "test/eh_11_SUITE"),
    Opts0 = ct_test_support:get_opts(Config),

    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],

    %% Running two cases separately creates two jobs in add_jobs.
    %% Each job runs init_per_suite, the case, end_per_suite.
    %% The resulting event streams are structurally identical —
    %% an event handler cannot tell where one job ends and the
    %% next begins without test_run_start.
    Opts = Opts0 ++ [{suite,Suite1},{testcase,[tc1,tc2]},
		     {event_handler,{eh_A,EvHArgs}}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(multiple_cases,
			       ct_test_support:reformat(Events, eh_A),
			       ?config(priv_dir, Config),
			       Opts),

    %% Without test_run_start, the two jobs produce:
    %%   init_per_suite, tc1, end_per_suite,
    %%   init_per_suite, tc2, end_per_suite
    %% There is no existing event that marks the job boundary.
    %% test_run_start provides that boundary.
    Tc1Match = fun(Data) when is_map(Data) ->
		       #{spec_name := SN} = Data,
		       case string:find(SN, "tc1") of
			   nomatch -> nomatch;
			   _ -> match
		       end
	       end,
    Tc2Match = fun(Data) when is_map(Data) ->
		       #{spec_name := SN} = Data,
		       case string:find(SN, "tc2") of
			   nomatch -> nomatch;
			   _ -> match
		       end
	       end,
    TestEvents =
	[{eh_A,start_logging,{'DEF','RUNDIR'}},
	 {eh_A,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
	 {eh_A,start_info,'_'},
	 %% First job: tc1
	 {eh_A,test_run_start,Tc1Match},
	 {eh_A,tc_start,{eh_11_SUITE,init_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,init_per_suite,ok}},
	 {eh_A,tc_start,{eh_11_SUITE,tc1}},
	 {eh_A,tc_done,{eh_11_SUITE,tc1,ok}},
	 {eh_A,test_stats,{1,0,{0,0}}},
	 {eh_A,tc_start,{eh_11_SUITE,end_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,end_per_suite,ok}},
	 %% Second job: tc2 — identical structure, only test_run_start differs
	 {eh_A,test_run_start,Tc2Match},
	 {eh_A,tc_start,{eh_11_SUITE,init_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,init_per_suite,ok}},
	 {eh_A,tc_start,{eh_11_SUITE,tc2}},
	 {eh_A,tc_done,{eh_11_SUITE,tc2,ok}},
	 {eh_A,test_stats,{2,0,{0,0}}},
	 {eh_A,tc_start,{eh_11_SUITE,end_per_suite}},
	 {eh_A,tc_done,{eh_11_SUITE,end_per_suite,ok}},
	 {eh_A,test_done,{'DEF','STOP_TIME'}},
	 {eh_A,stop_logging,[]}],

    ok = ct_test_support:verify_events(TestEvents++TestEvents, Events, Config).


group_path(doc) ->
    "When running a nested group path [outer,inner], the event handler "
    "sees init_per_group for each level but has no explicit indication "
    "that a group path was requested. test_run_start provides the full "
    "path in spec_name.";

group_path(suite) ->
    [];

group_path(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestObj = filename:join(DataDir, "event_handling_1"),
    Suite1 = filename:join(TestObj, "test/eh_12_SUITE"),
    Opts0 = ct_test_support:get_opts(Config),

    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],

    %% Run with a group path [outer, inner] — only inner's cases execute
    Opts = Opts0 ++ [{suite,Suite1},{group,[[outer,inner]]},
		     {event_handler,{eh_A,EvHArgs}}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(group_path,
			       ct_test_support:reformat(Events, eh_A),
			       ?config(priv_dir, Config),
			       Opts),

    %% The handler sees init_per_group for outer and inner, but only
    %% tc2 (which is in inner) runs. Without test_run_start, the handler
    %% cannot tell that the intent was to run path [outer,inner] vs.
    %% running the full outer group (which would also include tc1).
    %% test_run_start contains "outer.inner" in spec_name.
    PathMatch = fun(Data) when is_map(Data) ->
			#{spec_name := SN} = Data,
			case string:find(SN, "outer.inner") of
			    nomatch -> nomatch;
			    _ -> match
			end
		end,
    TestEvents =
	[{eh_A,start_logging,{'DEF','RUNDIR'}},
	 {eh_A,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
	 {eh_A,start_info,'_'},
	 {eh_A,test_run_start,PathMatch},
	 {eh_A,tc_start,{eh_12_SUITE,init_per_suite}},
	 {eh_A,tc_done,{eh_12_SUITE,init_per_suite,ok}},
	 [{eh_A,tc_start,{eh_12_SUITE,{init_per_group,outer,[]}}},
	  {eh_A,tc_done,{eh_12_SUITE,{init_per_group,outer,[]},ok}},
	  [{eh_A,tc_start,{eh_12_SUITE,{init_per_group,inner,[]}}},
	   {eh_A,tc_done,{eh_12_SUITE,{init_per_group,inner,[]},ok}},
	   {eh_A,tc_start,{eh_12_SUITE,tc2}},
	   {eh_A,tc_done,{eh_12_SUITE,tc2,ok}},
	   {eh_A,test_stats,{1,0,{0,0}}},
	   {eh_A,tc_start,{eh_12_SUITE,{end_per_group,inner,[]}}},
	   {eh_A,tc_done,{eh_12_SUITE,{end_per_group,inner,[]},ok}}],
	  {eh_A,tc_start,{eh_12_SUITE,{end_per_group,outer,[]}}},
	  {eh_A,tc_done,{eh_12_SUITE,{end_per_group,outer,[]},ok}}],
	 {eh_A,tc_start,{eh_12_SUITE,end_per_suite}},
	 {eh_A,tc_done,{eh_12_SUITE,end_per_suite,ok}},
	 {eh_A,test_done,{'DEF','STOP_TIME'}},
	 {eh_A,stop_logging,[]}],

    ok = ct_test_support:verify_events(TestEvents++TestEvents, Events, Config).


nested_groups(doc) ->
    [];

nested_groups(suite) ->
    [];

nested_groups(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),

    TestObj = filename:join(DataDir, "event_handling_1"),
    Suite1 = filename:join(TestObj, "test/eh_12_SUITE"),
    Opts0 = ct_test_support:get_opts(Config),

    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],

    Opts = Opts0 ++ [{suite,Suite1},
		     {event_handler,{eh_A,EvHArgs}}],

    ERPid = ct_test_support:start_event_receiver(Config),

    ok = ct_test_support:run(Opts, Config),

    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(nested_groups,
			       ct_test_support:reformat(Events, eh_A),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents =
	[{eh_A,start_logging,{'DEF','RUNDIR'}},
	 {eh_A,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
	 {eh_A,start_info,{1,1,2}},
	 {eh_A,tc_start,{eh_12_SUITE,init_per_suite}},
	 {eh_A,tc_done,{eh_12_SUITE,init_per_suite,ok}},
	 [{eh_A,tc_start,{eh_12_SUITE,{init_per_group,outer,[]}}},
	  {eh_A,tc_done,{eh_12_SUITE,{init_per_group,outer,[]},ok}},
	  {eh_A,tc_start,{eh_12_SUITE,tc1}},
	  {eh_A,tc_done,{eh_12_SUITE,tc1,ok}},
	  {eh_A,test_stats,{1,0,{0,0}}},
	  [{eh_A,tc_start,{eh_12_SUITE,{init_per_group,inner,[]}}},
	   {eh_A,tc_done,{eh_12_SUITE,{init_per_group,inner,[]},ok}},
	   {eh_A,tc_start,{eh_12_SUITE,tc2}},
	   {eh_A,tc_done,{eh_12_SUITE,tc2,ok}},
	   {eh_A,test_stats,{2,0,{0,0}}},
	   {eh_A,tc_start,{eh_12_SUITE,{end_per_group,inner,[]}}},
	   {eh_A,tc_done,{eh_12_SUITE,{end_per_group,inner,[]},ok}}],
	  {eh_A,tc_start,{eh_12_SUITE,{end_per_group,outer,[]}}},
	  {eh_A,tc_done,{eh_12_SUITE,{end_per_group,outer,[]},ok}}],
	 {eh_A,tc_start,{eh_12_SUITE,end_per_suite}},
	 {eh_A,tc_done,{eh_12_SUITE,end_per_suite,ok}},
	 {eh_A,test_done,{'DEF','STOP_TIME'}},
	 {eh_A,stop_logging,[]}],

    ok = ct_test_support:verify_events(TestEvents++TestEvents, Events, Config).


%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
