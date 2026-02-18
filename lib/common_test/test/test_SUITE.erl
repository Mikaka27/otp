-module(test_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

% all() -> [{group, group1}].
 
% groups() ->
%     [{group1, [], [{group_n1, [], [tc1_1, tc1_2, tc1_3, tc1_4, tc1_5, tc1_6, tc1_7, tc1_8, tc1_9, tc1_10]},
%                    {group_n2, [], [tc2_1, tc2_2, tc2_3, tc2_4, tc2_5, tc2_6, tc2_7, tc2_8, tc2_9, tc2_10]}]}].


all() ->
    [{group, group1},
     {group, group2}].

groups() ->
    % [{group1, [], [{subgroup1a, [], [test1a, test1b]},
    %                {subgroup1b, [], [test1c, test1d]}]},
    %  {group2, [], [{subgroup2a, [], [test2a, test2b]},
    %                {subgroup2b, [], [test2c, test2d]}]}].
    [{group1, [], [{group, subgroup1a},
                   {group, subgroup1b}]},
     {group2, [], [{group, subgroup2a},
                   {group, subgroup2b}]},
     {subgroup1a, [], [test1a, test1b]},
     {subgroup1b, [], [test1c, test1d]},
     {subgroup2a, [], [test2a, test2b]},
     {subgroup2b, [], [test2c, test2d]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

test1a(_Config) -> ok.
test1b(_Config) -> ok.
test1c(_Config) -> ok.
test1d(_Config) -> ok.
test2a(_Config) -> ok.
test2b(_Config) -> ok.
test2c(_Config) -> ok.
test2d(_Config) -> ok.
