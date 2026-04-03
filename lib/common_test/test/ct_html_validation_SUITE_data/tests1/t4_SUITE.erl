-module(t4_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


all() ->
    [{group, group1},
     {group, group2}].

groups() ->
    [{group1, [], [{subgroup1a, [], [test1a, test1b]},
                   {subgroup1b, [], [test1c, test1d]}]},
     {group2, [], [{subgroup2a, [], [test2a, test2b]},
                   {subgroup2b, [], [test2c, test2d]}]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(subgroup2a, _Config) ->
    ct:fail("end_per_group subgroup2a failure");
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
