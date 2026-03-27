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

-record(test, {
    test_name :: string(),
    suite_log_link :: string() | undefined,
    label :: atom() | undefined,
    start_date :: calendar:datetime(),
    ok :: non_neg_integer(),
    failed :: non_neg_integer(),
    skipped :: non_neg_integer(),
    user_skipped :: non_neg_integer(),
    auto_skipped :: non_neg_integer(),
    missing_suites :: non_neg_integer(),
    node :: node(),
    ct_log_link :: string(),
    old_runs_link :: string() | link | undefined
}).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    DataDir = ?config(data_dir, Config0),
    Config = ct_test_support:init_per_suite(Config0),
    SpecsDir = filename:join(DataDir, "specs"),
    [{specs_dir, SpecsDir} | Config].

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [run_spec_twice
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

run_spec_twice(Config) ->
    Specs = specs(?FUNCTION_NAME),

    setup_and_execute(?FUNCTION_NAME, specs(?FUNCTION_NAME), [], Config),

    % DataDir = ?config(data_dir, Config),
    % PrivDir = ?config(priv_dir, Config),

    % ct:pal("PrivDir: ~p~n", [PrivDir]),

    % TCName = ?FUNCTION_NAME,
    % TestID = {userconfig, {?MODULE, atom_to_list(TCName)}},
    % % TestTerms = [TestID,{spec,Specs},{label,TCName}] ++ TestOpts,

    % Opts0 = ct_test_support:get_opts(Config),
    % FName = fname(specs_dir, "all_suites_spec", Config),
    % % Specs = [{spec, [FName]}, {spec, [FName]}],
    % Specs = [TestID, {spec, [FName]}, {label, TCName}],

    % Opts = ct_test_support:get_overwritten_opts(Opts0 ++ Specs),

    % _ = ct_test_support:run_ct_run_test(Opts, Config),
    
    % Expected = #test{test_name = }
    % validate_html_files(PrivDir).

    %     ok ->
    %         ok;
    %     Error ->
    %         ct:fail("Test execution failed: ~p", [Error])
    % end.

% validate_html_output(Config) ->
%     DataDir = ?config(data_dir, Config),
%     PrivDir = ?config(priv_dir, Config),
    
%     %% Run a simple test to generate HTML output
%     Opts = ct_test_support:get_opts(Config),
%     TestOpts = [{dir, DataDir}, {logdir, PrivDir}],
%     ct:pal("DataDir: ~p, PrivDir: ~p~nOpts: ~p~nTestOpts: ~p~n", [DataDir, PrivDir, Opts, TestOpts]),
    
%     case ct_test_support:run(Opts ++ TestOpts, Config) of
%         ok ->
%             %% Validate HTML files in the log directory
%             validate_html_files(PrivDir);
%         Error ->
%             ct:fail("Test execution failed: ~p", [Error])
%     end.


%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

check_parameter(TCID) ->
    {ok, {config, TCID}}.

read_config(TCID) ->
    {ok, [{tcname, list_to_atom(TCID)}]}.

fname(Tag, File, Config) ->
    filename:join(?config(Tag, Config), File).

specs(run_spec_twice) ->
    [tests1_spec].

% -record(test, {
%     test_name :: string(),
%     suite_log_link :: string() | undefined,
%     label :: atom() | undefined,
%     start_date :: calendar:datetime(),
%     ok :: non_neg_integer(),
%     failed :: non_neg_integer(),
%     skipped :: non_neg_integer(),
%     user_skipped :: non_neg_integer(),
%     auto_skipped :: non_neg_integer(),
%     missing_suites :: non_neg_integer(),
%     node :: node(),
%     ct_log_link :: string(),
%     old_runs_link :: string() | link | undefined
% }).

get_default_expected(Config) ->
    #test{start_date = calendar:local_time(),
          node = ?config(ct_node, Config)}.

expected(tests1_spec, Label, Config) ->
    Test = get_default_expected(Config),
    [Test#test{test_name = ?config(data_dir, Config) ++ ".tests1",
              label = Label,
              ok = 32,
              failed = 0,
              skipped = 0,
              user_skipped = 0,
              auto_skipped = 0,
              missing_suites = 0
             }].

modify_expected(Expected0, run_spec_twice) ->
    WithLink = lists:map(fun(E) -> E#test{old_runs_link = link} end, Expected0),
    Duplicated = lists:foldl(fun(E, Acc) -> [E, E | Acc] end, [], Expected0),
    {WithLink, Duplicated}.

setup_and_execute(TCName, Specs, TestOpts, Config) ->
    TestID = {userconfig, {?MODULE, atom_to_list(TCName)}},
    TestTerms = [TestID, {spec, Specs}, {label, TCName}] ++ TestOpts,

    Opts0 = ct_test_support:get_opts(Config),
    Opts = ct_test_support:get_overwritten_opts(Opts0 ++ TestTerms),

    ct_test_support:run_ct_run_test(Opts, Config).

validate_html_files(LogDir) ->
    MainIndex = filename:join(LogDir, "index.html"),
    % Expected = #test{test_name = }
    validate_index_html_file(MainIndex, []).
    % %% Find all HTML files in the log directory
    % HtmlFiles = filelib:wildcard(filename:join(LogDir, "**/*.html")),
    % ct:pal("Found ~p HTML files to validate", [length(HtmlFiles)]),
    
    % %% Validate each HTML file
    % Results = [validate_html_file(File) || File <- HtmlFiles],
    
    % %% Check if all validations passed
    % case lists:all(fun(R) -> R =:= ok end, Results) of
    %     true -> ok;
    %     false -> ct:fail("HTML validation failed for some files")
    % end.

% validate_html_file(File) ->
%     ct:pal("File: ~p~n", [File]),
%     % Content = xmerl_scan:file(File),
%     % ct:pal("Content: ~p~n", [Content]),
%     % ok.
%     case file:read_file(File) of
%         {ok, Content} ->
%             %% Basic HTML validation
%             case validate_html_content(Content) of
%                 ok -> 
%                     ct:pal("HTML validation passed: ~s", [File]),
%                     ok;
%                 {error, Reason} ->
%                     ct:pal("HTML validation failed for ~s: ~p", [File, Reason]),
%                     error
%             end;
%         {error, Reason} ->
%             ct:pal("Failed to read file ~s: ~p", [File, Reason]),
%             error
%     end.

% validate_html_content(Content) ->
%     %% TODO: Implement actual HTML validation using xmerl or other parser
%     %% For now, just check basic structure
%     Parsed = xmerl_scan:string(binary_to_list(Content)),
%     ct:pal("Parsed: ~p~n", [Parsed]),
%     ok.
%     % ContentStr = binary_to_list(Content),
%     % case {string:find(ContentStr, "<html"), string:find(ContentStr, "</html>")} of
%     %     {nomatch, _} -> {error, missing_html_tag};
%     %     {_, nomatch} -> {error, missing_closing_html_tag};
%     %     _ -> ok
%     % end.

validate_index_html_file(Path, ExpectedTests) ->
    case file:open(Path, [read]) of
        {ok, Fd} ->
            {ok, _} = collect_until("<tbody>\n", file:read_line(Fd), Fd, []),
            {ok, Result} = collect_until("</tbody>\n", file:read_line(Fd), Fd, []),
            ct:pal("Result: ~p~n", [Result]),
            Tests = parse_tests(Result, []),
            ct:pal("Tests: ~p~n", [Tests]),
            Tests;
        Other ->
            Other
    end.

collect_until(Expected, {ok, Expected}, _Fd, Acc) ->
    {ok, lists:reverse(Acc)};
collect_until(Expected, {ok, Other}, Fd, Acc) ->
    collect_until(Expected, file:read_line(Fd), Fd, [Other | Acc]);
collect_until(_Expected, eof, _Fd, _Acc) ->
    {error, eof};
collect_until(_Expected, {error, Other}, _Fd, _Acc) ->
    {error, Other}.

parse_tests([], Tests) ->
    lists:reverse(Tests);
parse_tests(["<tr class=" ++ _ | Rest], Tests) ->
    parse_tests(Rest, [#test{} | Tests]);
parse_tests(["<td><a href=\"" ++ Line0 | Rest], [Test | Tests]) ->
    [Link, Line1] = string:split(Line0, "\">"),
    [Name, _] = string:split(Line1, "<"),
    case string:split(Line1, "<") of
        ["CT Log", _] ->
            parse_tests(Rest, [Test#test{ct_log_link = Link} | Tests]);
        [Name, _] ->
            parse_tests(Rest, [Test#test{test_name = Name, suite_log_link = Link} | Tests])
    end;
parse_tests(["<td align=center><b>" ++ Line | Rest], [Test | Tests]) ->
    [Label, _] = string:split(Line, "<"),
    parse_tests(Rest, [Test#test{label = list_to_atom(Label)} | Tests]);
parse_tests(["<td>" ++ Line | Rest], [#test{start_date = undefined} = Test | Tests]) ->
    [Date, _] = string:split(Line, "<"),
    {ok, [_DayOfWeek, MonthName, Day, Year, Hour, Minute, Second], _} =
        io_lib:fread("~s ~s ~d ~d ~d:~d:~d", Date),
    Month = month_name_to_number(MonthName),
    DateTime = {{Year, Month, Day}, {Hour, Minute, Second}},
    parse_tests(Rest, [Test#test{start_date = DateTime} | Tests]);
parse_tests(["<td align=right>" ++ Line | Rest], [#test{ok = undefined} = Test | Tests]) ->
    [Ok, _] = string:split(Line, "<"),
    parse_tests(Rest, [Test#test{ok = list_to_integer(Ok)} | Tests]);
parse_tests(["<td align=right>" ++ Line | Rest], [#test{failed = undefined} = Test | Tests]) ->
    [Failed, _] = string:split(Line, "<"),
    parse_tests(Rest, [Test#test{failed = list_to_integer(Failed)} | Tests]);
parse_tests(["<td align=right>" ++ Line | Rest], [#test{skipped = undefined} = Test0 | Tests]) ->
    [All, _] = string:split(Line, "<"),
    [Skipped, Other0] = string:split(All, "("),
    [UserSkipped, Other1] = string:split(Other0, "/"),
    [AutoSkipped, _] = string:split(Other1, ")"),
    Test = Test0#test{skipped = list_to_integer(string:trim(Skipped)),
                      user_skipped = list_to_integer(UserSkipped),
                      auto_skipped = list_to_integer(AutoSkipped)},
    parse_tests(Rest, [Test | Tests]);
parse_tests(["<td align=right>" ++ Line | Rest], [#test{missing_suites = undefined} = Test | Tests]) ->
    [MissingSuites, _] = string:split(Line, "<"),
    parse_tests(Rest, [Test#test{missing_suites = list_to_integer(MissingSuites)} | Tests]);
parse_tests(["<td align=right>" ++ Line | Rest], [#test{node = undefined} = Test | Tests]) ->
    [Node, _] = string:split(Line, "<"),
    parse_tests(Rest, [Test#test{node = list_to_atom(Node)} | Tests]);
parse_tests(["<td>none</td>\n" | Rest], [#test{old_runs_link = undefined} = Test | Tests]) ->
    parse_tests(Rest, [Test | Tests]);
parse_tests(["</tr>\n" | Rest], Tests) ->
    parse_tests(Rest, Tests).

month_name_to_number("Jan") -> 1;
month_name_to_number("Feb") -> 2;
month_name_to_number("Mar") -> 3;
month_name_to_number("Apr") -> 4;
month_name_to_number("May") -> 5;
month_name_to_number("Jun") -> 6;
month_name_to_number("Jul") -> 7;
month_name_to_number("Aug") -> 8;
month_name_to_number("Sep") -> 9;
month_name_to_number("Oct") -> 10;
month_name_to_number("Nov") -> 11;
month_name_to_number("Dec") -> 12.

compare_tests([#test{test_name = TestName,
                     label = Label,
                     start_date = Date1,
                     ok = Ok,
                     failed = Failed,
                     skipped = Skipped,
                     user_skipped = UserSkipped,
                     auto_skipped = AutoSkipped,
                     missing_suites = MissingSuites,
                     node = Node
                    } = Expected | Tests1],
              [#test{test_name = TestName,
                     suite_log_link = SuiteLink,
                     label = Label,
                     start_date = Date2,
                     ok = Ok,
                     failed = Failed,
                     skipped = Skipped,
                     user_skipped = UserSkipped,
                     auto_skipped = AutoSkipped,
                     missing_suites = MissingSuites,
                     node = Node,
                     ct_log_link = CtLogLink,
                     old_runs_link = OldRunsLink
                    } | Tests2]) ->
    ok = compare_date(Date1, Date2),
    ok = validate_links(Expected, SuiteLink, CtLogLink, OldRunsLink),
    compare_tests(Tests1, Tests2).

compare_date(Date, Date) ->
    ok;
compare_date(Date1, Date2) ->
    S1 = calendar:datetime_to_gregorian_seconds(Date1),
    S2 = calendar:datetime_to_gregorian_seconds(Date2),
    case abs(S2 - S1) of
        Diff when Diff =< 30 -> ok
    end.

validate_links(Expected, SuiteLink, CtLogLink, OldRunsLink) ->
    SuiteLinkParts = string:split(SuiteLink, "/", all),
    4 = length(SuiteLinkParts),
    ok = validate_link_parts(Expected, SuiteLinkParts),

    CtLogLinkParts = string:split(CtLogLink, "/", all),
    2 = length(CtLogLinkParts),
    ok = validate_link_parts(Expected, CtLogLinkParts).

validate_link_parts(_Expected, []) ->
    ok;
validate_link_parts(#test{start_date = ExpectedDate, node = ExpectedNode} = Expected,
                    ["ct_run." ++ Part | Rest]) ->
    [Node, Date] = string:split(Part, ".", all),
    ExpectedNode = list_to_atom(Node),
    ParsedDate = parse_link_date(Date),
    compare_date(ExpectedDate, ParsedDate),
    validate_link_parts(Expected, Rest);
validate_link_parts(#test{start_date = ExpectedDate} = Expected, ["run." ++ Date | Rest]) ->
    ParsedDate = parse_link_date(Date),
    compare_date(ExpectedDate, ParsedDate),
    validate_link_parts(Expected, Rest);
validate_link_parts(#test{test_name = TestName} = Expected, [Part | Rest]) ->
    Part = TestName ++ ".logs",
    validate_link_parts(Expected, Rest);
validate_link_parts(_Expected, ["suite.log.html" | []]) ->
    ok;
validate_link_parts(_Expected, ["ctlog.html" | []]) ->
    ok.

parse_link_date(Date) ->
    {ok, [Year, Month, Day, Hour, Minute, Second], _} = io_lib:fread("~d-~d-~d_~d.~d.~d", Date),
    {{Year, Month, Day}, {Hour, Minute, Second}}.
