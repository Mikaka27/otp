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

-module(diameter_test_inherit).

-compile(export_all).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_a.hrl").
-include("diameter_b.hrl").
-include("diameter_c.hrl").
-include("diameter_d.hrl").

run() ->
    H = #diameter_header{version = 1,
                         end_to_end_id = 1,
                         hop_by_hop_id = 1},
    Vs = [{'AAA', [0]},
          {'BBB', [1]},
          {'CCC', [?D_CCC_ONE]}],
    Pkt = #diameter_packet{header = H,
                           msg = Vs},

    [] = diameter_util:run([{?MODULE, [run, M, enc(M, Pkt)]}
                            || M <- ['ZR']]).

enc(M, #diameter_packet{msg = Vs} = P) ->
    diameter_codec:encode(diameter_d,
                          P#diameter_packet{msg = [M|Vs]}).

run(M, Pkt) ->
    dec(M, diameter_codec:decode(diameter_d, opts(M), Pkt)).

dec('ZR', #diameter_packet
    {msg = #d_ZR{'AAA' = [0],
                 'BBB' = [1],
                 'CCC' = [?D_CCC_ONE]}}) ->
    ok.

opts(Mod) ->
    #{app_dictionary => Mod,
      decode_format => record,
      string_decode => true,
      strict_mbit => true,
      rfc => 6733,
      failed_avp => false}.
