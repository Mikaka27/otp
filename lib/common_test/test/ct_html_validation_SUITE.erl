%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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
%%% File: ct_html_validation_SUITE
%%%
%%% Description:
%%% Test HTML validation
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_html_validation_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    Config1 = ct_test_support:init_per_suite(Config),
    SpecsDir1 = filename:join(DataDir, "specs1"),
    SpecsDir2 = filename:join(DataDir, "specs2"),
    [{specs_dir1,SpecsDir1},{specs_dir2,SpecsDir2} | Config1].

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [validate_html_output
    ].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%

validate_html_output(Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    
    %% Run a simple test to generate HTML output
    Opts = ct_test_support:get_opts(Config),
    TestOpts = [{dir, DataDir}, {logdir, PrivDir}],
    
    case ct_test_support:run(Opts ++ TestOpts, Config) of
        ok ->
            %% Validate HTML files in the log directory
            validate_html_files(PrivDir);
        Error ->
            ct:fail("Test execution failed: ~p", [Error])
    end.


%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

validate_html_files(LogDir) ->
    %% Find all HTML files in the log directory
    HtmlFiles = filelib:wildcard(filename:join(LogDir, "**/*.html")),
    ct:pal("Found ~p HTML files to validate", [length(HtmlFiles)]),
    
    %% Validate each HTML file
    Results = [validate_html_file(File) || File <- HtmlFiles],
    
    %% Check if all validations passed
    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> ok;
        false -> ct:fail("HTML validation failed for some files")
    end.

validate_html_file(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            %% Basic HTML validation
            case validate_html_content(Content) of
                ok -> 
                    ct:pal("HTML validation passed: ~s", [File]),
                    ok;
                {error, Reason} ->
                    ct:pal("HTML validation failed for ~s: ~p", [File, Reason]),
                    error
            end;
        {error, Reason} ->
            ct:pal("Failed to read file ~s: ~p", [File, Reason]),
            error
    end.

validate_html_content(Content) ->
    %% TODO: Implement actual HTML validation using xmerl or other parser
    %% For now, just check basic structure
    ContentStr = binary_to_list(Content),
    case {string:find(ContentStr, "<html"), string:find(ContentStr, "</html>")} of
        {nomatch, _} -> {error, missing_html_tag};
        {_, nomatch} -> {error, missing_closing_html_tag};
        _ -> ok
    end.
