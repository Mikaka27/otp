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

%%
%% Tests of the dictionary file compiler.
%%

-module(diameter_indirect_inherits_SUITE).

%% testcases, no common_test dependency
-export([run/0,
         run/1]).

%% common_test wrapping
-export([
         %% Framework functions
         suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
        
         %% The test cases
         test/1,
         verify_multiple_limited_imports_same_file/1,
         verify_multiple_whole_dict_imports_same_file/1,
         verify_multiple_limited_then_whole_dict_import_same_file/1,
         verify_both_limited_imports_are_kept_with_multiple_inherits/1,
         verify_multiple_limited_imports_are_resolved_when_overlapping/1,
         verify_limited_import_is_replaced_with_whole_dict_import/1,
         verify_whole_dict_import_is_not_replaced_with_limited_import/1,
         verify_enum_values_are_imported_along_the_inheritance_chain/1
        ]).

-include("diameter_util.hrl").


%% ===========================================================================

-define(base, "base_rfc3588.dia").
-define(S, atom_to_list).
-define(L, integer_to_list).

-define(CL(F),    ?CL(F, [])).
-define(CL(F, A), ?LOG("DCOMP", F, A)).

-define(OPTS, [erl, forms, return]).
-define(OPTS_INHERITS, ?OPTS ++ [indirect_inherits]).

-define(DEFAULT_AVP_NAMES, ['AAA', 'BBB', 'CCC']).

-define(AVP_HEADER(Name),
    case Name of
        'AAA' -> {111, 64, undefined};
        'BBB' -> {222, 0, undefined};
        'CCC' -> {333, 0, undefined};
        'DDD' -> {444, 0, undefined}
    end
).

-define(DICT(Name, Prefix),
    "@id 18\n"
    "@name " Name "\n"
    "@prefix " Prefix "\n"
).

-define(AVP_DICT_A, ?AVP_DICT_A([
    "AAA 111 Unsigned32 M",
    "BBB 222 Unsigned32 -",
    "CCC 333 Unsigned64 -"])).
-define(AVP_DICT_A(Avps),
    ?DICT("diameter_test_a", "a")
    "@avp_types\n" ++
    lists:join("\n", Avps)
).

-define(AVP_DICT_B(Inherits),
    ?DICT("diameter_test_b", "b") ++
    lists:join("\n", Inherits)
).

-define(AVP_DICT_C(Inherits),
    ?DICT("diameter_test_c", "c") ++
    lists:join("\n", Inherits)
).

-define(AVP_DICT_D(Inherits),
    ?DICT("diameter_test_d", "d") ++
    lists:join("\n", Inherits)
).

-define(ENUM_DICT_A,
    ?AVP_DICT_A ++
    "DDD 444 Enumerated -" ++
    "@enum DDD ZERO 0 ONE 1"
).

-define(ENUM_DICT_B,
    ?AVP_DICT_B(["@inherits diameter_test_a"]) ++
    "@enum DDD TWO 2 THREE 3"
).

-define(ENUM_DICT_C,
    ?AVP_DICT_C(["@inherits diameter_test_b"]) ++
    "@enum DDD FOUR 4 FIVE 5"
).

-define(ENUM_DICT_D,
    ?AVP_DICT_D(["@inherits diameter_test_c"]) ++
    "@enum DDD SIX 6 SEVEN 7"
).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 200}}].

all() ->
    [verify_multiple_limited_imports_same_file,
     verify_multiple_whole_dict_imports_same_file,
     verify_multiple_limited_then_whole_dict_import_same_file,
     verify_both_limited_imports_are_kept_with_multiple_inherits,
     verify_multiple_limited_imports_are_resolved_when_overlapping,
     verify_limited_import_is_replaced_with_whole_dict_import,
     verify_whole_dict_import_is_not_replaced_with_limited_import,
     verify_enum_values_are_imported_along_the_inheritance_chain
     ].

init_per_suite(Config) ->
    ?CL("init_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    ?DUTIL:init_per_suite(Config).

end_per_suite(Config) ->
    ?CL("end_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    ?DUTIL:end_per_suite(Config).


%% This test case can take a *long* time, so if the machine is too slow, skip
init_per_testcase(generate = Case, Config) when is_list(Config) ->
    ?CL("init_per_testcase(~w) -> check factor", [Case]),
    Key = dia_factor,
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Factor}} when (Factor > 10) ->
            ?CL("init_per_testcase(~w) -> Too slow (~w) => SKIP",
                [Case, Factor]),
            {skip, {machine_too_slow, Factor}};
        _ ->
            ?CL("init_per_testcase(~w) -> run test", [Case]),
            Config
    end;
init_per_testcase(Case, Config) ->
    ?CL("init_per_testcase(~w) -> entry", [Case]),
    Config.


end_per_testcase(Case, Config) when is_list(Config) ->
    ?CL("end_per_testcase(~w) -> entry", [Case]),
    Config.


%% ===========================================================================

%% run/0

run() ->
    run(all()).

%% run/1

run(List)
  when is_list(List) ->
    Tmp = ?MKTEMP("diameter_indirect_inherits"),
    try
        run(List, Tmp)
    after
        file:del_dir_r(Tmp)
    end.

%% run/2

run(List, Dir)
  when is_list(List) ->
    Path = filename:join([?LIB_DIR(diameter, src), "dict", ?base]),
    {ok, Bin} = file:read_file(Path),
    ?RUN([{{?MODULE, F, [{Bin, Dir}]}, 180000} || F <- List]);

run(F, Config) ->
    run([F], proplists:get_value(priv_dir, Config)).

%% ===========================================================================

load_forms(Forms) ->
    {ok, Mod, Bin, _} = compile:forms(Forms, [return]),
    {module, Mod} = code:load_binary(Mod, ?S(Mod), Bin),
    Mod.

%% ===========================================================================

verify_avps(M, PresentAvps) ->
    verify_avps(M, PresentAvps, []).

verify_avps(M, PresentAvps, NotPresentAvps) ->
    lists:foreach(fun(Avp) ->
        Header = ?AVP_HEADER(Avp),
        Header = M:avp_header(Avp)
    end, PresentAvps),
    lists:foreach(fun(Avp) ->
        {'EXIT', {badarg, _}} = catch M:avp_header(Avp)
    end, NotPresentAvps).

%% ===========================================================================

verify_enum_values(M, Name, PresentValues) ->
    verify_enum_values(M, Name, PresentValues, []).

verify_enum_values(M, Name, PresentValues, NotPresentValues) ->
    lists:foreach(fun(Value) ->
        <<0, 0, 0, Value>> = Encoded = M:enumerated_avp(encode, Name, Value),
        Value = M:enumerated_avp(decode, Name, Encoded)
    end, PresentValues),
    lists:foreach(fun(Value) ->
        {'EXIT', {badarg, _}} = catch M:enumerated_avp(encode, Name, Value),
        {'EXIT', {badarg, _}} = catch M:enumerated_avp(decode, Name, <<0, 0, 0, Value>>)
    end, NotPresentValues).

%% ===========================================================================

codec_list_of_dicts(Dicts) ->
    codec_list_of_dicts(Dicts, ?OPTS).

codec_list_of_dicts(Dicts, Opts) ->
    lists:foldl(fun(Dict, ignore) -> diameter_make:codec(Dict, Opts);
                   (Dict, Acc) -> Acc = diameter_make:codec(Dict, Opts)
                end, ignore, Dicts).

%% ===========================================================================

codec_list_of_options(Dict) ->
    codec_list_of_options(Dict, [?OPTS, ?OPTS_INHERITS]).

codec_list_of_options(Dict, ListsOfOpts) ->
    lists:foldl(fun(Opts, ignore) -> diameter_make:codec(Dict, Opts);
                      (Opts, Acc) -> Acc = diameter_make:codec(Dict, Opts)
                end, ignore, ListsOfOpts).

%% ===========================================================================

test(_) ->
    {ok, [E1, F1]} = diameter_make:codec(?AVP_DICT_A, ?OPTS),
    ct:pal("~s", [E1]),
    diameter_test_a = load_forms(F1).

%% ===========================================================================

verify_multiple_limited_imports_same_file(_) ->
    %% This test checks that when you inherit same avp twice you get avp_already_defined error.
    DictA = ?AVP_DICT_A,
    DictB = ?AVP_DICT_B(["@inherits diameter_test_a AAA", "@inherits diameter_test_a AAA"]),
    
    {ok, [EA, FA]}
        = diameter_make:codec(DictA, ?OPTS),
    ct:pal("~s", [EA]),
    diameter_test_a = load_forms(FA),

    {error, {avp_already_defined, _}} = codec_list_of_options(DictB).

%% ===========================================================================

verify_multiple_whole_dict_imports_same_file(_) ->
    %% This test checks that when you inherit same dictionary twice you get duplicate_import error.
    DictA = ?AVP_DICT_A,
    DictB = ?AVP_DICT_B(["@inherits diameter_test_a", "@inherits diameter_test_a"]),
    
    {ok, [EA, FA]}
        = diameter_make:codec(DictA, ?OPTS),
    ct:pal("~s", [EA]),
    diameter_test_a = load_forms(FA),

    {error, {duplicate_import, _}} = codec_list_of_options(DictB).

%% ===========================================================================

verify_multiple_limited_then_whole_dict_import_same_file(_) ->
    %% This test checks that when you inherit avp from dictionary, and then inherit whole
    %% dictionary, you get duplicate_import error.
    DictA = ?AVP_DICT_A,

    Inherits = [
        "@inherits diameter_test_a AAA",
        "@inherits diameter_test_a BBB",
        "@inherits diameter_test_a"
    ],
    DictB = ?AVP_DICT_B(Inherits),

    {ok, [EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s", [EA]),
    diameter_test_a = load_forms(FA),

    {error, {duplicate_import, _}} = codec_list_of_options(DictB).

%% ===========================================================================

verify_both_limited_imports_are_kept_with_multiple_inherits(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, when dict b inherits one avp from dict a
    %% then dict c inherits one avp from dict a AND inherits dict b, dict c and dict d should have
    %% inherited both avps of dict a.
    DictA = ?AVP_DICT_A,

    DictB = ?AVP_DICT_B(["@inherits diameter_test_a AAA"]),

    DictC = ?AVP_DICT_C(["@inherits diameter_test_a BBB", "@inherits diameter_test_b"]),
    %% Check reverse order as DictC
    DictC_R = ?AVP_DICT_C(["@inherits diameter_test_b", "@inherits diameter_test_a BBB"]),

    DictD = ?AVP_DICT_D(["@inherits diameter_test_c"]),
    
    {ok, [EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s", [EA]),
    diameter_test_a = load_forms(FA),

    {ok, [EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s", [EB]),
    diameter_test_b = load_forms(FB),

    {ok, [EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s", [EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ['AAA', 'BBB'], ['CCC']),

    {ok, [EC_R, FC_R]} = diameter_make:codec(DictC_R, ?OPTS_INHERITS),
    ct:pal("~s", [EC_R]),
    diameter_test_c = MC_R = load_forms(FC_R),
    verify_avps(MC_R, ['AAA', 'BBB'], ['CCC']),

    {ok, [ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s", [ED]),
    diameter_test_d = MD = load_forms(FD),
    verify_avps(MD, ['AAA', 'BBB'], ['CCC']).

%% ===========================================================================

verify_multiple_limited_imports_are_resolved_when_overlapping(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, when dict b inherits AAA and BBB from dict a,
    %% dict c inherits dict b and BBB and CCC from dict a, dict c and dict d should have resolved
    %% inherits, so that they see AAA, BBB, CCC
    DictA = ?AVP_DICT_A,

    DictB = ?AVP_DICT_B(["@inherits diameter_test_a AAA BBB"]),
    %% Check reverse order as DictB
    % DictB_R = ?AVP_DICT_B(["@inherits diameter_test_a BBB AAA"]),
    
    DictC = ?AVP_DICT_C(["@inherits diameter_test_a BBB CCC", "@inherits diameter_test_b"]),
    %% Check reverse order as DictB
    DictC_R = ?AVP_DICT_C(["@inherit diameter_test_b", "@inherits diameter_test_a BBB CCC"]),

    DictD = ?AVP_DICT_D(["@inherits diameter_test_c"]),

    {ok, [EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s", [EA]),
    diameter_test_a = MA = load_forms(FA),
    verify_avps(MA, ?DEFAULT_AVP_NAMES),

    {ok, [EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s", [EB]),
    diameter_test_b = MB = load_forms(FB),
    verify_avps(MB, ['AAA', 'BBB']),

    {ok, [EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s", [EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVP_NAMES),

    {ok, [EC_R, FC_R]} = diameter_make:codec(DictC_R, ?OPTS_INHERITS),
    ct:pal("~s", [EC_R]),
    diameter_test_c = MC_R = load_forms(FC_R),
    verify_avps(MC_R, ?DEFAULT_AVP_NAMES),

    {ok, [ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s", [ED]),
    diameter_test_d = MD = load_forms(FD),
    verify_avps(MD, ?DEFAULT_AVP_NAMES).

%% ===========================================================================

verify_limited_import_is_replaced_with_whole_dict_import(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, when dict b inherits one avp from dict a
    %% then dict c inherits whole dict a AND inherits dict b, dict c and dict d should have
    %% inherited all avps of dict a.
    DictA = ?AVP_DICT_A,

    DictB = ?AVP_DICT_B(["@inherits diameter_test_a AAA"]),

    DictC = ?AVP_DICT_C(["@inherits diameter_test_a", "@inherits diameter_test_b"]),
    %% Check reverse order as DictC
    DictC_R = ?AVP_DICT_C(["@inherits diameter_test_b", "@inherits diameter_test_a"]),

    DictD = ?AVP_DICT_D(["@inherits diameter_test_c"]),
    
    {ok, [EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s", [EA]),
    diameter_test_a = load_forms(FA),

    {ok, [EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s", [EB]),
    diameter_test_b = load_forms(FB),

    {ok, [EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s", [EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVP_NAMES),

    {ok, [EC_R, FC_R]} = diameter_make:codec(DictC_R, ?OPTS_INHERITS),
    ct:pal("~s", [EC_R]),
    diameter_test_c = MC_R = load_forms(FC_R),
    verify_avps(MC_R, ?DEFAULT_AVP_NAMES),

    {ok, [ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s", [ED]),
    diameter_test_d = MD = load_forms(FD),
    verify_avps(MD, ?DEFAULT_AVP_NAMES).

%% ===========================================================================

verify_whole_dict_import_is_not_replaced_with_limited_import(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, when dict b inherits whole dict a
    %% then dict c inherits one avp from dict a AND inherits dict b, dict c and dict d should have
    %% inherited all avps of dict a.
    DictA = ?AVP_DICT_A,

    DictB = ?AVP_DICT_B(["@inherits diameter_test_a"]),

    DictC = ?AVP_DICT_C(["@inherits diameter_test_a AAA", "@inherits diameter_test_b"]),
    %% Check reverse order as Dict3
    DictC_R = ?AVP_DICT_C(["@inherits diameter_test_b", "@inherits diameter_test_a AAA"]),

    DictD = ?AVP_DICT_D(["@inherits diameter_test_c"]),

    {ok, [EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s", [EA]),
    diameter_test_a = load_forms(FA),

    {ok, [EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s", [EB]),
    diameter_test_b = load_forms(FB),

    {ok, [EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s", [EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVP_NAMES),

    {ok, [EC_R, FC_R]} = diameter_make:codec(DictC_R, ?OPTS_INHERITS),
    ct:pal("~s", [EC_R]),
    diameter_test_c = MC_R = load_forms(FC_R),
    verify_avps(MC_R, ?DEFAULT_AVP_NAMES),

    {ok, [ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s", [ED]),
    diameter_test_d = MD = load_forms(FD),
    verify_avps(MD, ?DEFAULT_AVP_NAMES).

%% ===========================================================================

verify_enum_values_are_imported_along_the_inheritance_chain(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, when dict a defines an enum with 2 values,
    %% and then each dict in the chain inherits additional values, the last dict should
    %% have all enum values inherited.

    DictA = ?ENUM_DICT_A,
    DictB = ?ENUM_DICT_B,
    DictC = ?ENUM_DICT_C,
    DictD = ?ENUM_DICT_D,

    {ok, [EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s", [EA]),
    diameter_test_a = MA = load_forms(FA),
    verify_avps(MA, ?DEFAULT_AVP_NAMES ++ ['DDD']),
    verify_enum_values(MA, 'DDD', [0, 1], [2, 3, 4, 5, 6, 7]),

    {ok, [EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s", [EB]),
    diameter_test_b = MB = load_forms(FB),
    verify_avps(MB, ?DEFAULT_AVP_NAMES ++ ['DDD']),
    verify_enum_values(MB, 'DDD', [0, 1, 2, 3], [4, 5, 6, 7]),

    {ok, [EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s", [EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVP_NAMES ++ ['DDD']),
    verify_enum_values(MC, 'DDD', [0, 1, 2, 3, 4, 5], [6, 7]),

    {ok, [ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s", [ED]),
    diameter_test_d = MD = load_forms(FD),
    verify_avps(MD, ?DEFAULT_AVP_NAMES ++ ['DDD']),
    verify_enum_values(MD, 'DDD', [0, 1, 2, 3, 4, 5, 6, 7]).
