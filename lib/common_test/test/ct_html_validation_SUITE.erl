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

-define(NEXT_ROW(Text), string:find(Text, "<tr class")).
-define(NEXT_COL(Text), string:find(Text, "<td")).

-record(test, {
    test_name :: string(),
    suite_log_link :: string(),
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
    old_runs_link :: string() | link | undefined,
    elapsed_time :: calendar:time()
}).

-record(total, {
    ok :: non_neg_integer(),
    failed :: non_neg_integer(),
    skipped :: non_neg_integer(),
    user_skipped :: non_neg_integer(),
    auto_skipped :: non_neg_integer(),
    missing_suites :: non_neg_integer(),
    elapsed_time :: calendar:time()
}).

-record(test_case, {
    num :: pos_integer() | undefined,
    module :: module(),
    group :: atom() | undefined,
    tc :: atom(),
    tc_link :: string(),
    top_log :: string(),
    end_log :: string(),
    time :: calendar:time(),
    result :: atom(),
    comment :: [string()] | undefined
}).

-record(test_cases_total, {
    time :: calendar:time(),
    result :: atom(),
    ok :: non_neg_integer(),
    failed :: non_neg_integer(),
    skipped :: non_neg_integer(),
    total :: non_neg_integer(),
    elapsed_time :: calendar:time()
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
    Specs0 = specs(?FUNCTION_NAME),
    Specs1 = lists:append(Specs0, Specs0),
    Specs = [fname(specs_dir, Spec,Config) || Spec <- Specs1],

    setup_and_execute(?FUNCTION_NAME, Specs, [], Config).

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
    [tests1_spec,
     tests2_spec].

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

expected(Label, Config) ->
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

    ct_test_support:run_ct_run_test(Opts, Config),

    validate_html_files(Config, expected(TCName, Config)).

validate_html_files(Config, ExpectedTests) ->
    % Expected = #test{test_name = }
    validate_index_html_file(Config, ExpectedTests).
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

validate_index_html_file(Config, ExpectedTests) ->
    PrivDir = ?config(priv_dir, Config),
    Path = filename:join(PrivDir, "index.html"),
    {ok, Fd} = file:open(Path, [read]),
    try
        {ok, _} = collect_until("<tbody>\n", file:read_line(Fd), Fd, []),
        {ok, Result} = collect_until("</tbody>\n", file:read_line(Fd), Fd, []),
        ct:pal("Result: ~p~n", [Result]),
        Tests = parse_tests(Result, []),
        ct:pal("Tests: ~p~n", [Tests]),
        {ok, _} = collect_until("<tfoot>\n", file:read_line(Fd), Fd, []),
        {ok, Footer} = collect_until("</tfoot>\n", file:read_line(Fd), Fd, []),
        ct:pal("Footer: ~p~n", [Footer]),
        Total = parse_total(lists:flatten(Footer), 10, #total{}),
        ct:pal("Total: ~p~n", [Total]),
        lists:foreach(fun(#test{suite_log_link = SuiteLogLink, ct_log_link = CtLogLink} = Test) ->
            validate_suite_log_file(Config, SuiteLogLink, []),
            IndexLink = string:replace(CtLogLink, "ctlog.html", "index.html"),
            validate_specific_index_html_file(Config, IndexLink, [Test])
        end, Tests)
    after
        file:close(Fd)
    end.

validate_suite_log_file(Config, Link, ExpectedCases) ->
    PrivDir = ?config(priv_dir, Config),
    Path = filename:join(PrivDir, Link),
    {ok, Fd} = file:open(Path, [read]),
    try
        {ok, _} = collect_until("<tbody>\n", file:read_line(Fd), Fd, []),
        {ok, Result} = collect_until("</tbody>\n", file:read_line(Fd), Fd, []),
        ct:pal("Result: ~p~n", [Result]),
        Cases = parse_test_cases(lists:flatten(Result), 1, []),
        ct:pal("Cases: ~p~n", [Cases]),
        {ok, _} = collect_until("<tfoot>\n", file:read_line(Fd), Fd, []),
        {ok, Footer} = collect_until("</tfoot>\n", file:read_line(Fd), Fd, []),
        ct:pal("Footer: ~p~n", [Footer]),
        Total = parse_test_cases_total(lists:flatten(Footer), #test_cases_total{}),
        ct:pal("Total: ~p~n", [Total]),
        Cases
    after
        file:close(Fd)
    end.

validate_specific_index_html_file(Config, Link, ExpectedTests) ->
    PrivDir = ?config(priv_dir, Config),
    Path = filename:join(PrivDir, Link),
    {ok, Fd} = file:open(Path, [read]),
    try
        {ok, _} = collect_until("<tbody>\n", file:read_line(Fd), Fd, []),
        {ok, Content} = collect_until("</tbody>\n", file:read_line(Fd), Fd, []),
        ct:pal("Result: ~p~n", [Content]),
        Tests = parse_tests(Content, []),
        ct:pal("Tests: ~p~n", [Tests]),
        {ok, _} = collect_until("<tfoot>\n", file:read_line(Fd), Fd, []),
        {ok, Footer} = collect_until("</tfoot>\n", file:read_line(Fd), Fd, []),
        ct:pal("Footer: ~p~n", [Footer]),
        Total = parse_total(lists:flatten(Footer), 6, #total{}),
        ct:pal("Total: ~p~n", [Total])
    after
        file:close(Fd)
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
parse_tests(["<td><a href=\"" ++ Line0 | Rest], [#test{test_name = undefined} = Test | Tests]) ->
    [Link, Line1] = string:split(Line0, "\">"),
    [Name, _] = string:split(Line1, "<"),
    parse_tests(Rest, [Test#test{test_name = Name, suite_log_link = Link} | Tests]);
parse_tests(["<td><a href=\"" ++ Line0 | Rest], [#test{ct_log_link = undefined} = Test | Tests]) ->
    [Link, Line1] = string:split(Line0, "\">"),
    ["CT Log", _] = string:split(Line1, "<"),
    parse_tests(Rest, [Test#test{ct_log_link = Link} | Tests]);
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
parse_tests(["<td align=right>" ++ Line0 | Rest], [#test{failed = undefined} = Test | Tests]) ->
    Line = filter_font_color(Line0),
    [Failed, _] = string:split(Line, "<"),
    parse_tests(Rest, [Test#test{failed = list_to_integer(Failed)} | Tests]);
parse_tests(["<td align=right>" ++ Line0 | Rest], [#test{skipped = undefined} = Test0 | Tests]) ->
    Line = filter_font_color(Line0),
    [All, _] = string:split(Line, "<"),
    [Skipped, Other0] = string:split(All, "("),
    [UserSkipped, Other1] = string:split(Other0, "/"),
    [AutoSkipped, _] = string:split(Other1, ")"),
    Test = Test0#test{skipped = list_to_integer(string:trim(Skipped)),
                      user_skipped = list_to_integer(UserSkipped),
                      auto_skipped = list_to_integer(AutoSkipped)},
    parse_tests(Rest, [Test | Tests]);
parse_tests(["<td align=right>" ++ Line0 | Rest], [#test{missing_suites = undefined} = Test | Tests]) ->
    Line = filter_font_color(Line0),
    [MissingSuites, _] = string:split(Line, "<"),
    parse_tests(Rest, [Test#test{missing_suites = list_to_integer(MissingSuites)} | Tests]);
parse_tests(["<td align=right>" ++ Line | Rest], [#test{node = undefined, elapsed_time = undefined} = Test | Tests]) ->
    [NodeOrElapsedTime, _] = string:split(Line, "<"),
    case string:find(NodeOrElapsedTime, "@") of
        nomatch ->
            parse_tests(Rest, [Test#test{elapsed_time = parse_timestamp(NodeOrElapsedTime)} | Tests]);
        _ ->
            parse_tests(Rest, [Test#test{node = list_to_atom(NodeOrElapsedTime)} | Tests])
    end;
parse_tests(["<td><a href=\"" ++ Line0 | Rest], [#test{old_runs_link = undefined} = Test | Tests]) ->
    [Link, Line1] = string:split(Line0, "\">"),
    ["Old Runs", _] = string:split(Line1, "<"),
    parse_tests(Rest, [Test#test{old_runs_link = Link} | Tests]);
parse_tests(["<td>none</td>\n" | Rest], [#test{old_runs_link = undefined} = Test | Tests]) ->
    parse_tests(Rest, [Test | Tests]);
parse_tests(["</tr>\n" | Rest], Tests) ->
    parse_tests(Rest, Tests).

parse_total(nomatch, _Cols, Total) ->
    Total;
parse_total("<tr class=" ++ Rest, Cols, Total) ->
    parse_total(?NEXT_COL(Rest), Cols, Total);
parse_total("<td><b>Total</b></td>" ++ Rest, Cols, Total) when Cols =:= 6; Cols =:= 10 ->
    parse_total(?NEXT_COL(Rest), Cols, Total);
parse_total("<td>&nbsp;</td>" ++ Rest, Cols, Total) when Cols =:= 6; Cols =:= 10 ->
    parse_total(?NEXT_COL(Rest), Cols, Total);
parse_total("<td align=right><b>" ++ Rest0, Cols, #total{ok = undefined} = Total) ->
    [Ok, Rest] = string:split(Rest0, "</b>"),
    parse_total(?NEXT_COL(Rest), Cols, Total#total{ok = list_to_integer(Ok)});
parse_total("<td align=right><b>" ++ Rest0, Cols, #total{failed = undefined} = Total) ->
    [Failed, Rest] = string:split(Rest0, "</b>"),
    parse_total(?NEXT_COL(Rest), Cols, Total#total{failed = list_to_integer(Failed)});
parse_total("<td align=right>" ++ Rest0, Cols, #total{skipped = undefined} = Total0) ->
    [All, Rest] = string:split(Rest0, "<"),
    [Skipped, Other0] = string:split(All, "("),
    [UserSkipped, Other1] = string:split(Other0, "/"),
    [AutoSkipped, _] = string:split(Other1, ")"),
    Total = Total0#total{skipped = list_to_integer(string:trim(Skipped)),
                         user_skipped = list_to_integer(UserSkipped),
                         auto_skipped = list_to_integer(AutoSkipped)},
    parse_total(?NEXT_COL(Rest), Cols, Total);
parse_total("<td align=right><b>" ++ Rest0, Cols, #total{missing_suites = undefined} = Total) ->
    [MissingSuites, Rest] = string:split(Rest0, "</b>"),
    parse_total(?NEXT_COL(Rest), Cols, Total#total{missing_suites = list_to_integer(MissingSuites)});
parse_total("<td align=right><b>" ++ Rest0, 6 = Cols, #total{elapsed_time = undefined} = Total) ->
    [ElapsedTime, Rest] = string:split(Rest0, "</b>"),
    parse_total(?NEXT_COL(Rest), Cols, Total#total{elapsed_time = parse_timestamp(ElapsedTime)}).

parse_test_cases(nomatch, _, Cases) ->
    lists:reverse(Cases);
parse_test_cases("<tr class=" ++ Rest, _, Cases) ->
    parse_test_cases(?NEXT_COL(Rest), 1, [#test_case{} | Cases]);
parse_test_cases("<td><font color=\"black\">" ++ Rest0, 1 = Col, [Case | Cases]) ->
    case string:take(Rest0, lists:seq($0, $9)) of
        {[], Rest} ->
            parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case | Cases]);
        {Num, Rest} ->
            parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{num = list_to_integer(Num)} | Cases])
    end;
parse_test_cases("<td><font color=\"black\">" ++ Rest0, 2 = Col, [Case | Cases]) ->
    [Mod, Rest] = string:split(Rest0, "<"),
    parse_test_cases(string:find(Rest, "<td>"), Col + 1, [Case#test_case{module = Mod} | Cases]);
parse_test_cases("<td><font color=\"black\">" ++ Rest0, 3 = Col, [Case | Cases]) ->
    case string:split(Rest0, "<") of
        [[], Rest] ->
            parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case | Cases]);
        [Group, Rest] ->
            parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{group = Group} | Cases])
    end;
parse_test_cases("<td><a href=\"" ++ Rest0, 4 = Col, [Case | Cases]) ->
    [Link, Rest1] = string:split(Rest0, "\">"),
    [Name, Rest] = string:split(Rest1, "<"),
    parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{tc = Name, tc_link = Link} | Cases]);
parse_test_cases("<td><font color=\"black\">" ++ Rest0, 4 = Col, [Case | Cases]) ->
    [Name, Rest] = string:split(Rest0, "<"),
    parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{tc = Name} | Cases]);
parse_test_cases("<td><a href=\"" ++ Rest0, 5 = Col, [Case | Cases]) ->
    [Link1, Rest1] = string:split(Rest0, "\">"),
    "<a href=\"" ++ Rest2 = string:find(Rest1, "<a href=\""),
    [Link2, Rest] = string:split(Rest2, "\">"),
    parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{top_log = Link1, end_log = Link2} | Cases]);
parse_test_cases("<td><font color=\"black\">< ></font></td>" ++ Rest, 5 = Col, Cases) ->
    parse_test_cases(?NEXT_COL(Rest), Col + 1, Cases);
parse_test_cases("<td><font color=\"black\">" ++ Rest0, 6 = Col, [Case | Cases]) ->
    [Time, Rest] = string:split(Rest0, "<"),
    parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{time = parse_timestamp(Time)} | Cases]);
parse_test_cases("<td><font color=" ++ Rest0, 7 = Col, [Case | Cases]) ->
    [_, Rest1] = string:split(Rest0, ">"),
    [Result0, Rest] = string:split(Rest1, "<"),
    Result1 = string:lowercase(Result0),
    Result = list_to_atom(Result1),
    parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{result = Result} | Cases]);
parse_test_cases("<td>" ++ Rest0, 8 = _Col, [Case | Cases]) ->
    case string:split(Rest0, "</td>") of
        [[], Rest] ->
            parse_test_cases(?NEXT_ROW(Rest), 1, [Case | Cases]);
        [Comment0, Rest] ->
            Comment1 = string:split(Comment0, "<br />"),
            Comment = [filter_font_color(C) || C <- Comment1],
            parse_test_cases(?NEXT_ROW(Rest), 1, [Case#test_case{comment = Comment} | Cases])
    end.

parse_test_cases_total(nomatch, TCTotal) ->
    TCTotal;
parse_test_cases_total("<tr>" ++ Rest, TCTotal) ->
    parse_test_cases_total(?NEXT_COL(Rest), TCTotal);
parse_test_cases_total("<td><b>TOTAL</b></td>" ++ Rest, TCTotal) ->
    parse_test_cases_total(?NEXT_COL(Rest), TCTotal);
parse_test_cases_total("<td></td>" ++ Rest, TCTotal) ->
    parse_test_cases_total(?NEXT_COL(Rest), TCTotal);
parse_test_cases_total("<td>" ++ Rest0, #test_cases_total{time = undefined} = TCTotal) ->
    [Time, Rest] = string:split(Rest0, "<"),
    parse_test_cases_total(?NEXT_COL(Rest), TCTotal#test_cases_total{time = parse_timestamp(Time)});
parse_test_cases_total("<td><b>" ++ Rest0, #test_cases_total{result = undefined} = TCTotal) ->
    [Result0, Rest] = string:split(Rest0, "<"),
    Result = string:lowercase(Result0),
    parse_test_cases_total(?NEXT_COL(Rest), TCTotal#test_cases_total{result = list_to_atom(Result)});
parse_test_cases_total("<td>" ++ Rest0, #test_cases_total{ok = undefined} = TCTotal0) ->
    [Ok, Rest1] = string:split(Rest0, " Ok, "),
    [Failed, Rest2] = string:split(Rest1, " Failed, "),
    [Skipped, Rest3] = string:split(Rest2, " Skipped of "),
    [Total, Rest4] = string:split(Rest3, "<br>Elapsed Time: "),
    [ElapsedTime, Rest] = string:split(Rest4, "</td>"),
    TCTotal = TCTotal0#test_cases_total{ok = list_to_integer(Ok),
                                        failed = list_to_integer(Failed),
                                        skipped = list_to_integer(Skipped),
                                        total = list_to_integer(Total),
                                        elapsed_time = parse_timestamp(ElapsedTime)},
    parse_test_cases_total(?NEXT_COL(Rest), TCTotal).

filter_font_color(String0) ->
    String1 = re:replace(String0, "<font color=[^>]+>", ""),
    re:replace(String1, "</font>", "", [{return, list}]).

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

parse_timestamp(Str) ->
    {ok, [Seconds], _} = io_lib:fread("~fs", Str),
    MicroSecs = round(Seconds * 1_000_000),
    Megas = MicroSecs div 1_000_000_000_000,
    Secs = (MicroSecs div 1_000_000) rem 1_000_000,
    Micros = MicroSecs rem 1_000_000,
    {Megas, Secs, Micros}.

expected_test_cases(t1_SUITE) ->
    [#test_case{module = "t1_SUITE",
       tc = "init_per_suite",
       tc_link = "t1_suite.init_per_suite.html",
       top_log = "t1_suite.init_per_suite.html#top",
       end_log = "t1_suite.init_per_suite.html#end",
       result = ok},
     #test_case{module = "t1_SUITE",
       group = "group1",
       tc = "init_per_group",
       tc_link = "t1_suite.init_per_group.html",
       top_log = "t1_suite.init_per_group.html#top",
       end_log = "t1_suite.init_per_group.html#end",
       result = ok},
     #test_case{module = "t1_SUITE",
       group = "subgroup1a",
       tc = "init_per_group",
       tc_link = "t1_suite.init_per_group.2050.html",
       top_log = "t1_suite.init_per_group.2050.html#top",
       end_log = "t1_suite.init_per_group.2050.html#end",
       result = ok},
     #test_case{num = 1,
       module = "t1_SUITE",
       group = "subgroup1a",
       tc = "test1a",
       tc_link = "t1_suite.test1a.html",
       top_log = "t1_suite.test1a.html#top",
       end_log = "t1_suite.test1a.html#end",
       result = skipped,
       comment = ["&lt;&lt;\"Reason\"&gt;&gt;"]},
     #test_case{num = 2,
       module = "t1_SUITE",
       group = "subgroup1a",
       tc = "test1b",
       tc_link = "t1_suite.test1b.html",
       top_log = "t1_suite.test1b.html#top",
       end_log = "t1_suite.test1b.html#end",
       result = skipped,
       comment = ["init_per_testcase failed"]},
     #test_case{module = "t1_SUITE",
       group = "subgroup1a",
       tc = "end_per_group",
       tc_link = "t1_suite.end_per_group.html",
       top_log = "t1_suite.end_per_group.html#top",
       end_log = "t1_suite.end_per_group.html#end",
       result = ok},
     #test_case{module = "t1_SUITE",
       group = "subgroup1b",
       tc = "init_per_group",
       tc_link = "t1_suite.init_per_group.2114.html",
       top_log = "t1_suite.init_per_group.2114.html#top",
       end_log = "t1_suite.init_per_group.2114.html#end",
       result = ok},
     #test_case{num = 3,
       module = "t1_SUITE",
       group = "subgroup1b",
       tc = "test1c",
       tc_link = "t1_suite.test1c.html",
       top_log = "t1_suite.test1c.html#top",
       end_log = "t1_suite.test1c.html#end",
       result = ok},
     #test_case{num = 4,
       module = "t1_SUITE",
       group = "subgroup1b",
       tc = "test1d",
       tc_link = "t1_suite.test1d.html",
       top_log = "t1_suite.test1d.html#top",
       end_log = "t1_suite.test1d.html#end",
       result = ok},
     #test_case{module = "t1_SUITE",
       group = "subgroup1b",
       tc = "end_per_group",
       tc_link = "t1_suite.end_per_group.2178.html",
       top_log = "t1_suite.end_per_group.2178.html#top",
       end_log = "t1_suite.end_per_group.2178.html#end",
       result = ok},
     #test_case{module = "t1_SUITE",
       group = "group1",
       tc = "end_per_group",
       tc_link = "t1_suite.end_per_group.2242.html",
       top_log = "t1_suite.end_per_group.2242.html#top",
       end_log = "t1_suite.end_per_group.2242.html#end",
       result = ok},
     #test_case{module = "t1_SUITE",
       group = "group2",
       tc = "init_per_group",
       tc_link = "t1_suite.init_per_group.2306.html",
       top_log = "t1_suite.init_per_group.2306.html#top",
       end_log = "t1_suite.init_per_group.2306.html#end",
       result = ok},
     #test_case{module = "t1_SUITE",
       group = "subgroup2a",
       tc = "init_per_group",
       tc_link = "t1_suite.init_per_group.2370.html",
       top_log = "t1_suite.init_per_group.2370.html#top",
       end_log = "t1_suite.init_per_group.2370.html#end",
       result = ok},
     #test_case{num = 5,
       module = "t1_SUITE",
       group = "subgroup2a",
       tc = "test2a",
       tc_link = "t1_suite.test2a.html",
       top_log = "t1_suite.test2a.html#top",
       end_log = "t1_suite.test2a.html#end",
       result = ok},
     #test_case{num = 6,
       module = "t1_SUITE",
       group = "subgroup2a",
       tc = "test2b",
       tc_link = "t1_suite.test2b.html",
       top_log = "t1_suite.test2b.html#top",
       end_log = "t1_suite.test2b.html#end",
       result = ok},
     #test_case{module = "t1_SUITE",
       group = "subgroup2a",
       tc = "end_per_group",
       tc_link = "t1_suite.end_per_group.2434.html",
       top_log = "t1_suite.end_per_group.2434.html#top",
       end_log = "t1_suite.end_per_group.2434.html#end",
       result = ok},
     #test_case{module = "t1_SUITE",
       group = "subgroup2b",
       tc = "init_per_group",
       tc_link = "t1_suite.init_per_group.2498.html",
       top_log = "t1_suite.init_per_group.2498.html#top",
       end_log = "t1_suite.init_per_group.2498.html#end",
       result = ok},
     #test_case{num = 7,
       module = "t1_SUITE",
       group = "subgroup2b",
       tc = "test2c",
       tc_link = "t1_suite.test2c.html",
       top_log = "t1_suite.test2c.html#top",
       end_log = "t1_suite.test2c.html#end",
       result = ok},
     #test_case{num = 8,
       module = "t1_SUITE",
       group = "subgroup2b",
       tc = "test2d",
       tc_link = "t1_suite.test2d.html",
       top_log = "t1_suite.test2d.html#top",
       end_log = "t1_suite.test2d.html#end",
       result = ok},
     #test_case{module = "t1_SUITE",
       group = "subgroup2b",
       tc = "end_per_group",
       tc_link = "t1_suite.end_per_group.2562.html",
       top_log = "t1_suite.end_per_group.2562.html#top",
       end_log = "t1_suite.end_per_group.2562.html#end",
       result = ok},
     #test_case{module = "t1_SUITE",
       group = "group2",
       tc = "end_per_group",
       tc_link = "t1_suite.end_per_group.2626.html",
       top_log = "t1_suite.end_per_group.2626.html#top",
       end_log = "t1_suite.end_per_group.2626.html#end",
       result = ok},
     #test_case{module = "t1_SUITE",
       tc = "end_per_suite",
       tc_link = "t1_suite.end_per_suite.html",
       top_log = "t1_suite.end_per_suite.html#top",
       end_log = "t1_suite.end_per_suite.html#end",
       result = ok}];
expected_test_cases(t2_SUITE) ->
    [#test_case{module = "t2_SUITE",
       tc = "init_per_suite",
       tc_link = "t2_suite.init_per_suite.html",
       top_log = "t2_suite.init_per_suite.html#top",
       end_log = "t2_suite.init_per_suite.html#end",
       result = ok},
     #test_case{module = "t2_SUITE",
       group = "group1",
       tc = "init_per_group",
       tc_link = "t2_suite.init_per_group.html",
       top_log = "t2_suite.init_per_group.html#top",
       end_log = "t2_suite.init_per_group.html#end",
       result = ok},
     #test_case{module = "t2_SUITE",
       group = "subgroup1a",
       tc = "init_per_group",
       tc_link = "t2_suite.init_per_group.2690.html",
       top_log = "t2_suite.init_per_group.2690.html#top",
       end_log = "t2_suite.init_per_group.2690.html#end",
       result = ok},
     #test_case{num = 9,
       module = "t2_SUITE",
       group = "subgroup1a",
       tc = "test1a",
       tc_link = "t2_suite.test1a.html",
       top_log = "t2_suite.test1a.html#top",
       end_log = "t2_suite.test1a.html#end",
       result = failed,
       comment = ["{test_server,ts_tc,1796}",
       "{test_case_failed,deliberate failure}"]},
     #test_case{num = 10,
       module = "t2_SUITE",
       group = "subgroup1a",
       tc = "test1b",
       tc_link = "t2_suite.test1b.html",
       top_log = "t2_suite.test1b.html#top",
       end_log = "t2_suite.test1b.html#end",
       result = failed,
       comment = ["{t2_SUITE,test1b,<a href=\"t2_suite.src.html#37\">37</a>}",
       "{crash_on_purpose,[{t2_SUITE,...},{...}|...]}"]},
     #test_case{module = "t2_SUITE",
       group = "subgroup1a",
       tc = "end_per_group",
       tc_link = "t2_suite.end_per_group.html",
       top_log = "t2_suite.end_per_group.html#top",
       end_log = "t2_suite.end_per_group.html#end",
       result = ok},
     #test_case{module = "t2_SUITE",
       group = "subgroup1b",
       tc = "init_per_group",
       tc_link = "t2_suite.init_per_group.2754.html",
       top_log = "t2_suite.init_per_group.2754.html#top",
       end_log = "t2_suite.init_per_group.2754.html#end",
       result = ok},
     #test_case{num = 11,
       module = "t2_SUITE",
       group = "subgroup1b",
       tc = "test1c",
       tc_link = "t2_suite.test1c.html",
       top_log = "t2_suite.test1c.html#top",
       end_log = "t2_suite.test1c.html#end",
       result = ok},
     #test_case{num = 12,
       module = "t2_SUITE",
       group = "subgroup1b",
       tc = "test1d",
       tc_link = "t2_suite.test1d.html",
       top_log = "t2_suite.test1d.html#top",
       end_log = "t2_suite.test1d.html#end",
       result = ok},
     #test_case{module = "t2_SUITE",
       group = "subgroup1b",
       tc = "end_per_group",
       tc_link = "t2_suite.end_per_group.2818.html",
       top_log = "t2_suite.end_per_group.2818.html#top",
       end_log = "t2_suite.end_per_group.2818.html#end",
       result = ok},
     #test_case{module = "t2_SUITE",
       group = "group1",
       tc = "end_per_group",
       tc_link = "t2_suite.end_per_group.2882.html",
       top_log = "t2_suite.end_per_group.2882.html#top",
       end_log = "t2_suite.end_per_group.2882.html#end",
       result = ok},
     #test_case{module = "t2_SUITE",
       group = "group2",
       tc = "init_per_group",
       tc_link = "t2_suite.init_per_group.2946.html",
       top_log = "t2_suite.init_per_group.2946.html#top",
       end_log = "t2_suite.init_per_group.2946.html#end",
       result = ok},
     #test_case{module = "t2_SUITE",
       group = "subgroup2a",
       tc = "init_per_group",
       tc_link = "t2_suite.init_per_group.3010.html",
       top_log = "t2_suite.init_per_group.3010.html#top",
       end_log = "t2_suite.init_per_group.3010.html#end",
       result = ok},
     #test_case{num = 13,
       module = "t2_SUITE",
       group = "subgroup2a",
       tc = "test2a",
       tc_link = "t2_suite.test2a.html",
       top_log = "t2_suite.test2a.html#top",
       end_log = "t2_suite.test2a.html#end",
       result = ok},
     #test_case{num = 14,
       module = "t2_SUITE",
       group = "subgroup2a",
       tc = "test2b",
       tc_link = "t2_suite.test2b.html",
       top_log = "t2_suite.test2b.html#top",
       end_log = "t2_suite.test2b.html#end",
       result = ok},
     #test_case{module = "t2_SUITE",
       group = "subgroup2a",
       tc = "end_per_group",
       tc_link = "t2_suite.end_per_group.3074.html",
       top_log = "t2_suite.end_per_group.3074.html#top",
       end_log = "t2_suite.end_per_group.3074.html#end",
       result = ok},
     #test_case{module = "t2_SUITE",
       group = "subgroup2b",
       tc = "init_per_group",
       tc_link = "t2_suite.init_per_group.3138.html",
       top_log = "t2_suite.init_per_group.3138.html#top",
       end_log = "t2_suite.init_per_group.3138.html#end",
       result = ok},
     #test_case{num = 15,
       module = "t2_SUITE",
       group = "subgroup2b",
       tc = "test2c",
       tc_link = "t2_suite.test2c.html",
       top_log = "t2_suite.test2c.html#top",
       end_log = "t2_suite.test2c.html#end",
       result = ok},
     #test_case{num = 16,
       module = "t2_SUITE",
       group = "subgroup2b",
       tc = "test2d",
       tc_link = "t2_suite.test2d.html",
       top_log = "t2_suite.test2d.html#top",
       end_log = "t2_suite.test2d.html#end",
       result = ok},
     #test_case{module = "t2_SUITE",
       group = "subgroup2b",
       tc = "end_per_group",
       tc_link = "t2_suite.end_per_group.3202.html",
       top_log = "t2_suite.end_per_group.3202.html#top",
       end_log = "t2_suite.end_per_group.3202.html#end",
       result = ok},
     #test_case{module = "t2_SUITE",
       group = "group2",
       tc = "end_per_group",
       tc_link = "t2_suite.end_per_group.3266.html",
       top_log = "t2_suite.end_per_group.3266.html#top",
       end_log = "t2_suite.end_per_group.3266.html#end",
       result = ok},
     #test_case{module = "t2_SUITE",
       tc = "end_per_suite",
       tc_link = "t2_suite.end_per_suite.html",
       top_log = "t2_suite.end_per_suite.html#top",
       end_log = "t2_suite.end_per_suite.html#end",
       result = ok}];
expected_test_cases(t3_SUITE) ->
    [#test_case{module = "t3_SUITE",
       tc = "init_per_suite",
       tc_link = "t3_suite.init_per_suite.html",
       top_log = "t3_suite.init_per_suite.html#top",
       end_log = "t3_suite.init_per_suite.html#end",
       result = ok},
     #test_case{module = "t3_SUITE",
       group = "group1",
       tc = "init_per_group",
       tc_link = "t3_suite.init_per_group.html",
       top_log = "t3_suite.init_per_group.html#top",
       end_log = "t3_suite.init_per_group.html#end",
       result = ok},
     #test_case{module = "t3_SUITE",
       group = "subgroup1a",
       tc = "init_per_group",
       tc_link = "t3_suite.init_per_group.3330.html",
       top_log = "t3_suite.init_per_group.3330.html#top",
       end_log = "t3_suite.init_per_group.3330.html#end",
       result = failed,
       comment = ["{test_server,ts_tc,1796}",
       "{test_case_failed,init_per_group subgroup1a failure}"]},
     #test_case{num = 17,
       module = "t3_SUITE",
       group = "subgroup1a",
       tc = "test1a",
       result = skipped,
       comment = ["init_per_group failed"]},
     #test_case{num = 18,
       module = "t3_SUITE",
       group = "subgroup1a",
       tc = "test1b",
       result = skipped,
       comment = ["init_per_group failed"]},
     #test_case{module = "t3_SUITE",
       group = "subgroup1a",
       tc = "end_per_group",
       result = skipped,
       comment = ["init_per_group failed"]},
     #test_case{module = "t3_SUITE",
       group = "subgroup1b",
       tc = "init_per_group",
       tc_link = "t3_suite.init_per_group.3394.html",
       top_log = "t3_suite.init_per_group.3394.html#top",
       end_log = "t3_suite.init_per_group.3394.html#end",
       result = ok},
     #test_case{num = 19,
       module = "t3_SUITE",
       group = "subgroup1b",
       tc = "test1c",
       tc_link = "t3_suite.test1c.html",
       top_log = "t3_suite.test1c.html#top",
       end_log = "t3_suite.test1c.html#end",
       result = ok},
     #test_case{num = 20,
       module = "t3_SUITE",
       group = "subgroup1b",
       tc = "test1d",
       tc_link = "t3_suite.test1d.html",
       top_log = "t3_suite.test1d.html#top",
       end_log = "t3_suite.test1d.html#end",
       result = ok},
     #test_case{module = "t3_SUITE",
       group = "subgroup1b",
       tc = "end_per_group",
       tc_link = "t3_suite.end_per_group.html",
       top_log = "t3_suite.end_per_group.html#top",
       end_log = "t3_suite.end_per_group.html#end",
       result = ok},
     #test_case{module = "t3_SUITE",
       group = "group1",
       tc = "end_per_group",
       tc_link = "t3_suite.end_per_group.3458.html",
       top_log = "t3_suite.end_per_group.3458.html#top",
       end_log = "t3_suite.end_per_group.3458.html#end",
       result = ok},
     #test_case{module = "t3_SUITE",
       group = "group2",
       tc = "init_per_group",
       tc_link = "t3_suite.init_per_group.3522.html",
       top_log = "t3_suite.init_per_group.3522.html#top",
       end_log = "t3_suite.init_per_group.3522.html#end",
       result = ok},
     #test_case{module = "t3_SUITE",
       group = "subgroup2a",
       tc = "init_per_group",
       tc_link = "t3_suite.init_per_group.3586.html",
       top_log = "t3_suite.init_per_group.3586.html#top",
       end_log = "t3_suite.init_per_group.3586.html#end",
       result = ok},
     #test_case{num = 21,
       module = "t3_SUITE",
       group = "subgroup2a",
       tc = "test2a",
       tc_link = "t3_suite.test2a.html",
       top_log = "t3_suite.test2a.html#top",
       end_log = "t3_suite.test2a.html#end",
       result = ok},
     #test_case{num = 22,
       module = "t3_SUITE",
       group = "subgroup2a",
       tc = "test2b",
       tc_link = "t3_suite.test2b.html",
       top_log = "t3_suite.test2b.html#top",
       end_log = "t3_suite.test2b.html#end",
       result = ok},
     #test_case{module = "t3_SUITE",
       group = "subgroup2a",
       tc = "end_per_group",
       tc_link = "t3_suite.end_per_group.3650.html",
       top_log = "t3_suite.end_per_group.3650.html#top",
       end_log = "t3_suite.end_per_group.3650.html#end",
       result = ok},
     #test_case{module = "t3_SUITE",
       group = "subgroup2b",
       tc = "init_per_group",
       tc_link = "t3_suite.init_per_group.3714.html",
       top_log = "t3_suite.init_per_group.3714.html#top",
       end_log = "t3_suite.init_per_group.3714.html#end",
       result = ok},
     #test_case{num = 23,
       module = "t3_SUITE",
       group = "subgroup2b",
       tc = "test2c",
       tc_link = "t3_suite.test2c.html",
       top_log = "t3_suite.test2c.html#top",
       end_log = "t3_suite.test2c.html#end",
       result = ok},
     #test_case{num = 24,
       module = "t3_SUITE",
       group = "subgroup2b",
       tc = "test2d",
       tc_link = "t3_suite.test2d.html",
       top_log = "t3_suite.test2d.html#top",
       end_log = "t3_suite.test2d.html#end",
       result = ok},
     #test_case{module = "t3_SUITE",
       group = "subgroup2b",
       tc = "end_per_group",
       tc_link = "t3_suite.end_per_group.3778.html",
       top_log = "t3_suite.end_per_group.3778.html#top",
       end_log = "t3_suite.end_per_group.3778.html#end",
       result = ok},
     #test_case{module = "t3_SUITE",
       group = "group2",
       tc = "end_per_group",
       tc_link = "t3_suite.end_per_group.3842.html",
       top_log = "t3_suite.end_per_group.3842.html#top",
       end_log = "t3_suite.end_per_group.3842.html#end",
       result = ok},
     #test_case{module = "t3_SUITE",
       tc = "end_per_suite",
       tc_link = "t3_suite.end_per_suite.html",
       top_log = "t3_suite.end_per_suite.html#top",
       end_log = "t3_suite.end_per_suite.html#end",
       result = ok}];
expected_test_cases(t4_SUITE) ->
    [#test_case{module = "t4_SUITE",
       tc = "init_per_suite",
       tc_link = "t4_suite.init_per_suite.html",
       top_log = "t4_suite.init_per_suite.html#top",
       end_log = "t4_suite.init_per_suite.html#end",
       result = ok},
     #test_case{module = "t4_SUITE",
       group = "group1",
       tc = "init_per_group",
       tc_link = "t4_suite.init_per_group.html",
       top_log = "t4_suite.init_per_group.html#top",
       end_log = "t4_suite.init_per_group.html#end",
       result = ok},
     #test_case{module = "t4_SUITE",
       group = "subgroup1a",
       tc = "init_per_group",
       tc_link = "t4_suite.init_per_group.3906.html",
       top_log = "t4_suite.init_per_group.3906.html#top",
       end_log = "t4_suite.init_per_group.3906.html#end",
       result = ok},
     #test_case{num = 25,
       module = "t4_SUITE",
       group = "subgroup1a",
       tc = "test1a",
       tc_link = "t4_suite.test1a.html",
       top_log = "t4_suite.test1a.html#top",
       end_log = "t4_suite.test1a.html#end",
       result = ok},
     #test_case{num = 26,
       module = "t4_SUITE",
       group = "subgroup1a",
       tc = "test1b",
       tc_link = "t4_suite.test1b.html",
       top_log = "t4_suite.test1b.html#top",
       end_log = "t4_suite.test1b.html#end",
       result = ok},
     #test_case{module = "t4_SUITE",
       group = "subgroup1a",
       tc = "end_per_group",
       tc_link = "t4_suite.end_per_group.html",
       top_log = "t4_suite.end_per_group.html#top",
       end_log = "t4_suite.end_per_group.html#end",
       result = ok},
     #test_case{module = "t4_SUITE",
       group = "subgroup1b",
       tc = "init_per_group",
       tc_link = "t4_suite.init_per_group.3970.html",
       top_log = "t4_suite.init_per_group.3970.html#top",
       end_log = "t4_suite.init_per_group.3970.html#end",
       result = ok},
     #test_case{num = 27,
       module = "t4_SUITE",
       group = "subgroup1b",
       tc = "test1c",
       tc_link = "t4_suite.test1c.html",
       top_log = "t4_suite.test1c.html#top",
       end_log = "t4_suite.test1c.html#end",
       result = ok},
     #test_case{num = 28,
       module = "t4_SUITE",
       group = "subgroup1b",
       tc = "test1d",
       tc_link = "t4_suite.test1d.html",
       top_log = "t4_suite.test1d.html#top",
       end_log = "t4_suite.test1d.html#end",
       result = ok},
     #test_case{module = "t4_SUITE",
       group = "subgroup1b",
       tc = "end_per_group",
       tc_link = "t4_suite.end_per_group.4034.html",
       top_log = "t4_suite.end_per_group.4034.html#top",
       end_log = "t4_suite.end_per_group.4034.html#end",
       result = ok},
     #test_case{module = "t4_SUITE",
       group = "group1",
       tc = "end_per_group",
       tc_link = "t4_suite.end_per_group.4098.html",
       top_log = "t4_suite.end_per_group.4098.html#top",
       end_log = "t4_suite.end_per_group.4098.html#end",
       result = ok},
     #test_case{module = "t4_SUITE",
       group = "group2",
       tc = "init_per_group",
       tc_link = "t4_suite.init_per_group.4162.html",
       top_log = "t4_suite.init_per_group.4162.html#top",
       end_log = "t4_suite.init_per_group.4162.html#end",
       result = ok},
     #test_case{module = "t4_SUITE",
       group = "subgroup2a",
       tc = "init_per_group",
       tc_link = "t4_suite.init_per_group.4226.html",
       top_log = "t4_suite.init_per_group.4226.html#top",
       end_log = "t4_suite.init_per_group.4226.html#end",
       result = ok},
     #test_case{num = 29,
       module = "t4_SUITE",
       group = "subgroup2a",
       tc = "test2a",
       tc_link = "t4_suite.test2a.html",
       top_log = "t4_suite.test2a.html#top",
       end_log = "t4_suite.test2a.html#end",
       result = ok},
     #test_case{num = 30,
       module = "t4_SUITE",
       group = "subgroup2a",
       tc = "test2b",
       tc_link = "t4_suite.test2b.html",
       top_log = "t4_suite.test2b.html#top",
       end_log = "t4_suite.test2b.html#end",
       result = ok},
     #test_case{module = "t4_SUITE",
       group = "subgroup2a",
       tc = "end_per_group",
       tc_link = "t4_suite.end_per_group.4290.html",
       top_log = "t4_suite.end_per_group.4290.html#top",
       end_log = "t4_suite.end_per_group.4290.html#end",
       result = failed,
       comment = ["{test_server,ts_tc,1796}",
       "{test_case_failed,end_per_group subgroup2a failure}"]},
     #test_case{module = "t4_SUITE",
       group = "subgroup2b",
       tc = "init_per_group",
       tc_link = "t4_suite.init_per_group.4354.html",
       top_log = "t4_suite.init_per_group.4354.html#top",
       end_log = "t4_suite.init_per_group.4354.html#end",
       result = ok},
     #test_case{num = 31,
       module = "t4_SUITE",
       group = "subgroup2b",
       tc = "test2c",
       tc_link = "t4_suite.test2c.html",
       top_log = "t4_suite.test2c.html#top",
       end_log = "t4_suite.test2c.html#end",
       result = ok},
     #test_case{num = 32,
       module = "t4_SUITE",
       group = "subgroup2b",
       tc = "test2d",
       tc_link = "t4_suite.test2d.html",
       top_log = "t4_suite.test2d.html#top",
       end_log = "t4_suite.test2d.html#end",
       result = ok},
     #test_case{module = "t4_SUITE",
       group = "subgroup2b",
       tc = "end_per_group",
       tc_link = "t4_suite.end_per_group.4418.html",
       top_log = "t4_suite.end_per_group.4418.html#top",
       end_log = "t4_suite.end_per_group.4418.html#end",
       result = ok},
     #test_case{module = "t4_SUITE",
       group = "group2",
       tc = "end_per_group",
       tc_link = "t4_suite.end_per_group.4482.html",
       top_log = "t4_suite.end_per_group.4482.html#top",
       end_log = "t4_suite.end_per_group.4482.html#end",
       result = ok},
     #test_case{module = "t4_SUITE",
       tc = "end_per_suite",
       tc_link = "t4_suite.end_per_suite.html",
       top_log = "t4_suite.end_per_suite.html#top",
       end_log = "t4_suite.end_per_suite.html#end",
       result = ok}];
expected_test_cases(t5_SUITE) ->
    [#test_case{module = "t5_SUITE",
       tc = "init_per_suite",
       tc_link = "t5_suite.init_per_suite.html",
       top_log = "t5_suite.init_per_suite.html#top",
       end_log = "t5_suite.init_per_suite.html#end",
       result = failed,
       comment = ["{test_server,ts_tc,1796}",
       "{test_case_failed,init_per_suite failure}"]},
     #test_case{num = 1,
       module = "t5_SUITE",
       group = "subgroup1a",
       tc = "test1a",
       result = skipped,
       comment = ["init_per_suite failed"]},
     #test_case{num = 2,
       module = "t5_SUITE",
       group = "subgroup1a",
       tc = "test1b",
       result = skipped,
       comment = ["init_per_suite failed"]},
     #test_case{num = 3,
       module = "t5_SUITE",
       group = "subgroup1b",
       tc = "test1c",
       result = skipped,
       comment = ["init_per_suite failed"]},
     #test_case{num = 4,
       module = "t5_SUITE",
       group = "subgroup1b",
       tc = "test1d",
       result = skipped,
       comment = ["init_per_suite failed"]},
     #test_case{num = 5,
       module = "t5_SUITE",
       group = "subgroup2a",
       tc = "test2a",
       result = skipped,
       comment = ["init_per_suite failed"]},
     #test_case{num = 6,
       module = "t5_SUITE",
       group = "subgroup2a",
       tc = "test2b",
       result = skipped,
       comment = ["init_per_suite failed"]},
     #test_case{num = 7,
       module = "t5_SUITE",
       group = "subgroup2b",
       tc = "test2c",
       result = skipped,
       comment = ["init_per_suite failed"]},
     #test_case{num = 8,
       module = "t5_SUITE",
       group = "subgroup2b",
       tc = "test2d",
       result = skipped,
       comment = ["init_per_suite failed"]},
     #test_case{module = "t5_SUITE",
       tc = "end_per_suite",
       result = skipped,
       comment = ["init_per_suite failed"]}];
expected_test_cases(t6_SUITE) ->
    [#test_case{module = "t6_SUITE",
       tc = "init_per_suite",
       tc_link = "t6_suite.init_per_suite.html",
       top_log = "t6_suite.init_per_suite.html#top",
       end_log = "t6_suite.init_per_suite.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       group = "group1",
       tc = "init_per_group",
       tc_link = "t6_suite.init_per_group.html",
       top_log = "t6_suite.init_per_group.html#top",
       end_log = "t6_suite.init_per_group.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       group = "subgroup1a",
       tc = "init_per_group",
       tc_link = "t6_suite.init_per_group.4546.html",
       top_log = "t6_suite.init_per_group.4546.html#top",
       end_log = "t6_suite.init_per_group.4546.html#end",
       result = ok},
     #test_case{num = 9,
       module = "t6_SUITE",
       group = "subgroup1a",
       tc = "test1a",
       tc_link = "t6_suite.test1a.html",
       top_log = "t6_suite.test1a.html#top",
       end_log = "t6_suite.test1a.html#end",
       result = ok},
     #test_case{num = 10,
       module = "t6_SUITE",
       group = "subgroup1a",
       tc = "test1b",
       tc_link = "t6_suite.test1b.html",
       top_log = "t6_suite.test1b.html#top",
       end_log = "t6_suite.test1b.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       group = "subgroup1a",
       tc = "end_per_group",
       tc_link = "t6_suite.end_per_group.html",
       top_log = "t6_suite.end_per_group.html#top",
       end_log = "t6_suite.end_per_group.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       group = "subgroup1b",
       tc = "init_per_group",
       tc_link = "t6_suite.init_per_group.4610.html",
       top_log = "t6_suite.init_per_group.4610.html#top",
       end_log = "t6_suite.init_per_group.4610.html#end",
       result = ok},
     #test_case{num = 11,
       module = "t6_SUITE",
       group = "subgroup1b",
       tc = "test1c",
       tc_link = "t6_suite.test1c.html",
       top_log = "t6_suite.test1c.html#top",
       end_log = "t6_suite.test1c.html#end",
       result = ok},
     #test_case{num = 12,
       module = "t6_SUITE",
       group = "subgroup1b",
       tc = "test1d",
       tc_link = "t6_suite.test1d.html",
       top_log = "t6_suite.test1d.html#top",
       end_log = "t6_suite.test1d.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       group = "subgroup1b",
       tc = "end_per_group",
       tc_link = "t6_suite.end_per_group.4674.html",
       top_log = "t6_suite.end_per_group.4674.html#top",
       end_log = "t6_suite.end_per_group.4674.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       group = "group1",
       tc = "end_per_group",
       tc_link = "t6_suite.end_per_group.4738.html",
       top_log = "t6_suite.end_per_group.4738.html#top",
       end_log = "t6_suite.end_per_group.4738.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       group = "group2",
       tc = "init_per_group",
       tc_link = "t6_suite.init_per_group.4802.html",
       top_log = "t6_suite.init_per_group.4802.html#top",
       end_log = "t6_suite.init_per_group.4802.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       group = "subgroup2a",
       tc = "init_per_group",
       tc_link = "t6_suite.init_per_group.4866.html",
       top_log = "t6_suite.init_per_group.4866.html#top",
       end_log = "t6_suite.init_per_group.4866.html#end",
       result = ok},
     #test_case{num = 13,
       module = "t6_SUITE",
       group = "subgroup2a",
       tc = "test2a",
       tc_link = "t6_suite.test2a.html",
       top_log = "t6_suite.test2a.html#top",
       end_log = "t6_suite.test2a.html#end",
       result = ok},
     #test_case{num = 14,
       module = "t6_SUITE",
       group = "subgroup2a",
       tc = "test2b",
       tc_link = "t6_suite.test2b.html",
       top_log = "t6_suite.test2b.html#top",
       end_log = "t6_suite.test2b.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       group = "subgroup2a",
       tc = "end_per_group",
       tc_link = "t6_suite.end_per_group.4930.html",
       top_log = "t6_suite.end_per_group.4930.html#top",
       end_log = "t6_suite.end_per_group.4930.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       group = "subgroup2b",
       tc = "init_per_group",
       tc_link = "t6_suite.init_per_group.4994.html",
       top_log = "t6_suite.init_per_group.4994.html#top",
       end_log = "t6_suite.init_per_group.4994.html#end",
       result = ok},
     #test_case{num = 15,
       module = "t6_SUITE",
       group = "subgroup2b",
       tc = "test2c",
       tc_link = "t6_suite.test2c.html",
       top_log = "t6_suite.test2c.html#top",
       end_log = "t6_suite.test2c.html#end",
       result = ok},
     #test_case{num = 16,
       module = "t6_SUITE",
       group = "subgroup2b",
       tc = "test2d",
       tc_link = "t6_suite.test2d.html",
       top_log = "t6_suite.test2d.html#top",
       end_log = "t6_suite.test2d.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       group = "subgroup2b",
       tc = "end_per_group",
       tc_link = "t6_suite.end_per_group.5058.html",
       top_log = "t6_suite.end_per_group.5058.html#top",
       end_log = "t6_suite.end_per_group.5058.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       group = "group2",
       tc = "end_per_group",
       tc_link = "t6_suite.end_per_group.5122.html",
       top_log = "t6_suite.end_per_group.5122.html#top",
       end_log = "t6_suite.end_per_group.5122.html#end",
       result = ok},
     #test_case{module = "t6_SUITE",
       tc = "end_per_suite",
       tc_link = "t6_suite.end_per_suite.html",
       top_log = "t6_suite.end_per_suite.html#top",
       end_log = "t6_suite.end_per_suite.html#end",
       result = failed,
       comment = ["{test_server,ts_tc,1796}",
       "{test_case_failed,end_per_suite failure}"]}
    ];
expected_test_cases(t7_SUITE) ->
    [#test_case{module = "t7_SUITE",
       tc = "init_per_suite",
       tc_link = "t7_suite.init_per_suite.html",
       top_log = "t7_suite.init_per_suite.html#top",
       end_log = "t7_suite.init_per_suite.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       group = "group1",
       tc = "init_per_group",
       tc_link = "t7_suite.init_per_group.html",
       top_log = "t7_suite.init_per_group.html#top",
       end_log = "t7_suite.init_per_group.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       group = "subgroup1a",
       tc = "init_per_group",
       tc_link = "t7_suite.init_per_group.5186.html",
       top_log = "t7_suite.init_per_group.5186.html#top",
       end_log = "t7_suite.init_per_group.5186.html#end",
       result = ok},
     #test_case{num = 17,
       module = "t7_SUITE",
       group = "subgroup1a",
       tc = "test1a",
       tc_link = "t7_suite.test1a.html",
       top_log = "t7_suite.test1a.html#top",
       end_log = "t7_suite.test1a.html#end",
       result = ok,
       comment = ["WARNING: end_per_testcase crashed!\n"]},
     #test_case{num = 18,
       module = "t7_SUITE",
       group = "subgroup1a",
       tc = "test1b",
       tc_link = "t7_suite.test1b.html",
       top_log = "t7_suite.test1b.html#top",
       end_log = "t7_suite.test1b.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       group = "subgroup1a",
       tc = "end_per_group",
       tc_link = "t7_suite.end_per_group.html",
       top_log = "t7_suite.end_per_group.html#top",
       end_log = "t7_suite.end_per_group.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       group = "subgroup1b",
       tc = "init_per_group",
       tc_link = "t7_suite.init_per_group.5250.html",
       top_log = "t7_suite.init_per_group.5250.html#top",
       end_log = "t7_suite.init_per_group.5250.html#end",
       result = ok},
     #test_case{num = 19,
       module = "t7_SUITE",
       group = "subgroup1b",
       tc = "test1c",
       tc_link = "t7_suite.test1c.html",
       top_log = "t7_suite.test1c.html#top",
       end_log = "t7_suite.test1c.html#end",
       result = ok},
     #test_case{num = 20,
       module = "t7_SUITE",
       group = "subgroup1b",
       tc = "test1d",
       tc_link = "t7_suite.test1d.html",
       top_log = "t7_suite.test1d.html#top",
       end_log = "t7_suite.test1d.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       group = "subgroup1b",
       tc = "end_per_group",
       tc_link = "t7_suite.end_per_group.5314.html",
       top_log = "t7_suite.end_per_group.5314.html#top",
       end_log = "t7_suite.end_per_group.5314.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       group = "group1",
       tc = "end_per_group",
       tc_link = "t7_suite.end_per_group.5378.html",
       top_log = "t7_suite.end_per_group.5378.html#top",
       end_log = "t7_suite.end_per_group.5378.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       group = "group2",
       tc = "init_per_group",
       tc_link = "t7_suite.init_per_group.5442.html",
       top_log = "t7_suite.init_per_group.5442.html#top",
       end_log = "t7_suite.init_per_group.5442.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       group = "subgroup2a",
       tc = "init_per_group",
       tc_link = "t7_suite.init_per_group.5506.html",
       top_log = "t7_suite.init_per_group.5506.html#top",
       end_log = "t7_suite.init_per_group.5506.html#end",
       result = ok},
     #test_case{num = 21,
       module = "t7_SUITE",
       group = "subgroup2a",
       tc = "test2a",
       tc_link = "t7_suite.test2a.html",
       top_log = "t7_suite.test2a.html#top",
       end_log = "t7_suite.test2a.html#end",
       result = ok},
     #test_case{num = 22,
       module = "t7_SUITE",
       group = "subgroup2a",
       tc = "test2b",
       tc_link = "t7_suite.test2b.html",
       top_log = "t7_suite.test2b.html#top",
       end_log = "t7_suite.test2b.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       group = "subgroup2a",
       tc = "end_per_group",
       tc_link = "t7_suite.end_per_group.5570.html",
       top_log = "t7_suite.end_per_group.5570.html#top",
       end_log = "t7_suite.end_per_group.5570.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       group = "subgroup2b",
       tc = "init_per_group",
       tc_link = "t7_suite.init_per_group.5634.html",
       top_log = "t7_suite.init_per_group.5634.html#top",
       end_log = "t7_suite.init_per_group.5634.html#end",
       result = ok},
     #test_case{num = 23,
       module = "t7_SUITE",
       group = "subgroup2b",
       tc = "test2c",
       tc_link = "t7_suite.test2c.html",
       top_log = "t7_suite.test2c.html#top",
       end_log = "t7_suite.test2c.html#end",
       result = ok},
     #test_case{num = 24,
       module = "t7_SUITE",
       group = "subgroup2b",
       tc = "test2d",
       tc_link = "t7_suite.test2d.html",
       top_log = "t7_suite.test2d.html#top",
       end_log = "t7_suite.test2d.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       group = "subgroup2b",
       tc = "end_per_group",
       tc_link = "t7_suite.end_per_group.5698.html",
       top_log = "t7_suite.end_per_group.5698.html#top",
       end_log = "t7_suite.end_per_group.5698.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       group = "group2",
       tc = "end_per_group",
       tc_link = "t7_suite.end_per_group.5762.html",
       top_log = "t7_suite.end_per_group.5762.html#top",
       end_log = "t7_suite.end_per_group.5762.html#end",
       result = ok},
     #test_case{module = "t7_SUITE",
       tc = "end_per_suite",
       tc_link = "t7_suite.end_per_suite.html",
       top_log = "t7_suite.end_per_suite.html#top",
       end_log = "t7_suite.end_per_suite.html#end",
       result = ok}];
expected_test_cases(t8_SUITE) ->
    [#test_case{module = "t8_SUITE",
       tc = "init_per_suite",
       tc_link = "t8_suite.init_per_suite.html",
       top_log = "t8_suite.init_per_suite.html#top",
       end_log = "t8_suite.init_per_suite.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       group = "group1",
       tc = "init_per_group",
       tc_link = "t8_suite.init_per_group.html",
       top_log = "t8_suite.init_per_group.html#top",
       end_log = "t8_suite.init_per_group.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       group = "subgroup1a",
       tc = "init_per_group",
       tc_link = "t8_suite.init_per_group.5826.html",
       top_log = "t8_suite.init_per_group.5826.html#top",
       end_log = "t8_suite.init_per_group.5826.html#end",
       result = ok},
     #test_case{num = 25,
       module = "t8_SUITE",
       group = "subgroup1a",
       tc = "test1a",
       tc_link = "t8_suite.test1a.html",
       top_log = "t8_suite.test1a.html#top",
       end_log = "t8_suite.test1a.html#end",
       result = skipped,
       comment = ["init_per_testcase failed"]},
     #test_case{num = 26,
       module = "t8_SUITE",
       group = "subgroup1a",
       tc = "test1b",
       tc_link = "t8_suite.test1b.html",
       top_log = "t8_suite.test1b.html#top",
       end_log = "t8_suite.test1b.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       group = "subgroup1a",
       tc = "end_per_group",
       tc_link = "t8_suite.end_per_group.html",
       top_log = "t8_suite.end_per_group.html#top",
       end_log = "t8_suite.end_per_group.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       group = "subgroup1b",
       tc = "init_per_group",
       tc_link = "t8_suite.init_per_group.5890.html",
       top_log = "t8_suite.init_per_group.5890.html#top",
       end_log = "t8_suite.init_per_group.5890.html#end",
       result = ok},
     #test_case{num = 27,
       module = "t8_SUITE",
       group = "subgroup1b",
       tc = "test1c",
       tc_link = "t8_suite.test1c.html",
       top_log = "t8_suite.test1c.html#top",
       end_log = "t8_suite.test1c.html#end",
       result = ok},
     #test_case{num = 28,
       module = "t8_SUITE",
       group = "subgroup1b",
       tc = "test1d",
       tc_link = "t8_suite.test1d.html",
       top_log = "t8_suite.test1d.html#top",
       end_log = "t8_suite.test1d.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       group = "subgroup1b",
       tc = "end_per_group",
       tc_link = "t8_suite.end_per_group.5954.html",
       top_log = "t8_suite.end_per_group.5954.html#top",
       end_log = "t8_suite.end_per_group.5954.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       group = "group1",
       tc = "end_per_group",
       tc_link = "t8_suite.end_per_group.6018.html",
       top_log = "t8_suite.end_per_group.6018.html#top",
       end_log = "t8_suite.end_per_group.6018.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       group = "group2",
       tc = "init_per_group",
       tc_link = "t8_suite.init_per_group.6082.html",
       top_log = "t8_suite.init_per_group.6082.html#top",
       end_log = "t8_suite.init_per_group.6082.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       group = "subgroup2a",
       tc = "init_per_group",
       tc_link = "t8_suite.init_per_group.6146.html",
       top_log = "t8_suite.init_per_group.6146.html#top",
       end_log = "t8_suite.init_per_group.6146.html#end",
       result = ok},
     #test_case{num = 29,
       module = "t8_SUITE",
       group = "subgroup2a",
       tc = "test2a",
       tc_link = "t8_suite.test2a.html",
       top_log = "t8_suite.test2a.html#top",
       end_log = "t8_suite.test2a.html#end",
       result = failed,
       comment = ["{t8_SUITE,test2a,<a href=\"t8_suite.src.html#42\">42</a>}",
       "{test2a_crash,[{t8_SUITE,...},{...}|...]}"]},
     #test_case{num = 30,
       module = "t8_SUITE",
       group = "subgroup2a",
       tc = "test2b",
       tc_link = "t8_suite.test2b.html",
       top_log = "t8_suite.test2b.html#top",
       end_log = "t8_suite.test2b.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       group = "subgroup2a",
       tc = "end_per_group",
       tc_link = "t8_suite.end_per_group.6210.html",
       top_log = "t8_suite.end_per_group.6210.html#top",
       end_log = "t8_suite.end_per_group.6210.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       group = "subgroup2b",
       tc = "init_per_group",
       tc_link = "t8_suite.init_per_group.6274.html",
       top_log = "t8_suite.init_per_group.6274.html#top",
       end_log = "t8_suite.init_per_group.6274.html#end",
       result = ok},
     #test_case{num = 31,
       module = "t8_SUITE",
       group = "subgroup2b",
       tc = "test2c",
       tc_link = "t8_suite.test2c.html",
       top_log = "t8_suite.test2c.html#top",
       end_log = "t8_suite.test2c.html#end",
       result = ok},
     #test_case{num = 32,
       module = "t8_SUITE",
       group = "subgroup2b",
       tc = "test2d",
       tc_link = "t8_suite.test2d.html",
       top_log = "t8_suite.test2d.html#top",
       end_log = "t8_suite.test2d.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       group = "subgroup2b",
       tc = "end_per_group",
       tc_link = "t8_suite.end_per_group.6338.html",
       top_log = "t8_suite.end_per_group.6338.html#top",
       end_log = "t8_suite.end_per_group.6338.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       group = "group2",
       tc = "end_per_group",
       tc_link = "t8_suite.end_per_group.6402.html",
       top_log = "t8_suite.end_per_group.6402.html#top",
       end_log = "t8_suite.end_per_group.6402.html#end",
       result = ok},
     #test_case{module = "t8_SUITE",
       tc = "end_per_suite",
       tc_link = "t8_suite.end_per_suite.html",
       top_log = "t8_suite.end_per_suite.html#top",
       end_log = "t8_suite.end_per_suite.html#end",
       result = ok}].
