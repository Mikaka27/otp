%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

-module(ct_results_validator).

-include("ct_results_parser.hrl").

-export([validate_total/1,
         validate_test_cases_total/1]).

%%%-----------------------------------------------------------------
%%% EXPORTED FUNCTIONS
%%%-----------------------------------------------------------------

validate_total(Tests) ->
    Total = #total{ok = 0,
                   failed = 0,
                   skipped = 0,
                   user_skipped = 0,
                   auto_skipped = 0,
                   missing_suites = 0},
    validate_total(Tests, Total).

validate_test_cases_total(TestCases) ->
    Total = #test_cases_total{ok = 0,
                              failed = 0,
                              skipped = 0,
                              total = 0
                             },
    validate_test_cases_total(TestCases, Total).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

validate_total([#total{} = Total | []], Total) ->
    ok;
validate_total([#test{ok = Ok, failed = F, skipped = S, user_skipped = US, auto_skipped = AS,
                      missing_suites = MS, elapsed_time = ET} | Tests],
               #total{ok = TOk, failed = TF, skipped = TS, user_skipped = TUS, auto_skipped = TAS,
                      missing_suites = TMS, elapsed_time = TET} = Total0) ->
    Total = Total0#total{ok = TOk + Ok,
                         failed = TF + F,
                         skipped = TS + S,
                         user_skipped = TUS + US,
                         auto_skipped = TAS + AS,
                         missing_suites = TMS + MS,
                         elapsed_time = add_time(TET, ET)},
    validate_total(Tests, Total).

%%%-----------------------------------------------------------------

validate_test_cases_total([#test_cases_total{ok = Ok, failed = F, skipped = S, total = Tot,
                                             time = T, elapsed_time = ET} | []],
                          #test_cases_total{ok = Ok, failed = F, skipped = S, time = T, total = Tot})
  when ET /= undefined ->
    ok;
validate_test_cases_total([#test_case{time = Time, tc = Tc} | Cases], #test_cases_total{time = TotalTime0} = Total0)
  when Tc == init_per_suite; Tc == init_per_group; Tc == end_per_group; Tc == end_per_suite ->
    TotalTime = add_time(TotalTime0, Time),
    Total = Total0#test_cases_total{time = TotalTime},
    validate_test_cases_total(Cases, Total);
validate_test_cases_total([#test_case{result = Result, time = Time} | Cases],
                          #test_cases_total{time = TotalTime0} = Total0) ->
    Total1 = update_result(Result, Total0),
    TotalTime = add_time(TotalTime0, Time),
    Total = Total1#test_cases_total{time = TotalTime},
    validate_test_cases_total(Cases, Total).

%%%-----------------------------------------------------------------

add_time(undefined, undefined) ->
    undefined;
add_time(undefined, T) ->
    T;
add_time(T1, T2) ->
    S1 = calendar:time_to_seconds(T1),
    S2 = calendar:time_to_seconds(T2),
    calendar:seconds_to_time(S1 + S2).

%%%-----------------------------------------------------------------

update_result(ok, #test_cases_total{ok = Ok, total = Tot, result = undefined} = Total) ->
    Total#test_cases_total{ok = Ok + 1, total = Tot + 1, result = ok};
update_result(skipped, #test_cases_total{skipped = S, total = Tot, result = undefined} = Total) ->
    Total#test_cases_total{skipped = S + 1, total = Tot + 1, result = skipped};
update_result(ok, #test_cases_total{ok = Ok, total = Tot} = Total) ->
    Total#test_cases_total{ok = Ok + 1, total = Tot + 1};
update_result(skipped, #test_cases_total{skipped = S, total = Tot} = Total) ->
    Total#test_cases_total{skipped = S + 1, total = Tot + 1};
update_result(failed, #test_cases_total{failed = F, total = Tot} = Total) ->
    Total#test_cases_total{failed = F + 1, total = Tot + 1, result = failed}.
