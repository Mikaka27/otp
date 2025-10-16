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
%% Tests of the diameter peer discovery protocol.
%%

-module(diameter_peer_discovery_SUITE).

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
         test/1
        ]).

-include("diameter_util.hrl").


%% ===========================================================================

-define(base, "base_rfc3588.dia").
-define(S, atom_to_list).
-define(L, integer_to_list).

-define(CL(F),    ?CL(F, [])).
-define(CL(F, A), ?LOG("DPD", F, A)).

-define(RUN_NS, "run-ns").
-define(LOG_FILE, "ns.log").

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 200}}].

all() ->
    [test].

init_per_suite(Config) ->
    ?CL("init_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    ?DUTIL:init_per_suite(Config).

end_per_suite(Config) ->
    ?CL("end_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    ?DUTIL:end_per_suite(Config).


% init_per_testcase(Case, Config) ->
%     ?CL("init_per_testcase(~w) -> entry", [Case]),
%     Config.


% end_per_testcase(Case, Config) when is_list(Config) ->
%     ?CL("end_per_testcase(~w) -> entry", [Case]),
%     Config.


init_per_testcase(Func, Config) ->

    ?CL("init_per_testcase -> entry with"
       "~n      Func:   ~p"
       "~n      Config: ~p", [Func, Config]),

    PrivDir = proplists:get_value(priv_dir, Config),
    DataDir = proplists:get_value(data_dir, Config),
    try ns_init(zone_dir(Func), PrivDir, DataDir) of
	NsSpec ->
            UpdatedConfig =
                [{nameserver, NsSpec}] ++
                case Func of
                    basic ->
                        Config;
                    _ ->
                        ?CL("init_per_testcase -> get resolver lookup"),
                        Lookup = inet_db:res_option(lookup),
                        ?CL("init_per_testcase -> set file:dns"),
                        inet_db:set_lookup([file,dns]),
                        case NsSpec of
                            {_,{IP,Port},_} ->
                                ?CL("init_per_testcase -> "
                                   "insert alt nameserver ~p:~w",
                                   [IP, Port]),
                                inet_db:ins_alt_ns(IP, Port);
                            _ -> ok
                        end,
                        ?CL("init_per_testcase -> saved "
                           "lookup: ~p", [Lookup]),
                        [{res_lookup, Lookup} | Config]
                end,
            %% dbg:tracer(),
            %% dbg:p(all, c),
            %% dbg:tpl(inet_res, query_nss_res, cx),
            ?CL("init_per_testcase -> done:"
               "~n    NsSpec: ~p", [NsSpec]),
            UpdatedConfig
    catch
	SkipReason ->
            ?CL("init_per_testcase -> skip: ~p", [SkipReason]),
	    {skip, SkipReason};
        Class : Reason : Stacktrace ->
            ?CL("init_per_testcase -> ~w: ~p"
               "~n    ~p~n", [Class, Reason, Stacktrace]),
            {fail, Reason}
    end.

end_per_testcase(_Func, Config) ->
    NsSpec = proplists:get_value(nameserver, Config),
    case proplists:lookup(res_lookup, Config) of
        none ->
            ok;
        {res_lookup, Lookup} ->
            inet_db:set_lookup(Lookup),
            case NsSpec of
                {_,{IP,Port},_} ->
                    inet_db:del_alt_ns(IP, Port);
                _ -> ok
            end
    end,
    %% dbg:stop(),
    ns_end(NsSpec, proplists:get_value(priv_dir, Config)).


%% ===========================================================================

%% run/0

run() ->
    run(all()).

%% run/1

run(List)
  when is_list(List) ->
    Tmp = ?MKTEMP("diameter_peer_discovery"),
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

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Nameserver control

ns(Config) ->
    {_ZoneDir,NS,_P} = proplists:get_value(nameserver, Config),
    NS.

ns_init(ZoneDir, PrivDir, DataDir) ->

    ?CL("ns_init -> entry with"
       "~n      ZoneDir: ~p"
       "~n      PrivDir: ~p"
       "~n      DataDir: ~p", [ZoneDir, PrivDir, DataDir]),

    case {os:type(),ZoneDir} of
	{{unix,_},undefined} ->
            ?CL("ns_init -> nothing"),
            undefined;
	{{unix,_},otptest} ->
            ?CL("ns_init -> prepare start"),
	    PortNum = case {os:type(),os:version()} of
			  {{unix,solaris},{M,V,_}} when M =< 5, V < 10 ->
			      11895 + rand:uniform(100);
			  _ ->
			      {ok, S} = gen_udp:open(0, [{reuseaddr,true}]),
			      {ok, PNum} = inet:port(S),
			      gen_udp:close(S),
			      PNum
		      end,
            ?CL("ns_init -> use port number ~p", [PortNum]),
	    RunNamed = filename:join(DataDir, ?RUN_NS),
        %% TODO: Fix makefile to preserve execute permissions instead!
        ok = file:change_mode(RunNamed, 8#00777),
            ?CL("ns_init -> use named ~p", [RunNamed]),
	    NS = {{127,0,0,1},PortNum},
            ?CL("ns_init -> try open port (exec)"),
	    P = erlang:open_port({spawn_executable,RunNamed},
				 [{cd,PrivDir},
				  {line,80},
				  {args,["127.0.0.1",
					 integer_to_list(PortNum),
					 atom_to_list(ZoneDir)]},
                                  {env,[{"LOGNAME",os:getenv("LOGNAME",os:getenv("USER"))}]},
				  stderr_to_stdout,
				  eof]),
            ?CL("ns_init -> port ~p", [P]),
	    ns_start(ZoneDir, PrivDir, NS, P);
	_ ->
	    throw("Only run on Unix")
    end.

ns_start(ZoneDir, PrivDir, NS, P) ->

    ?CL("ns_start -> await message"),

    case ns_collect(P) of
	eof ->
            ?CL("ns_start -> eof"),
	    erlang:error(eof);
	"Running: "++_ ->
            ?CL("ns_start -> running"),
	    {ZoneDir,NS,P};
	"Skip: "++Reason ->
            ?CL("ns_start -> skip: "
               "~n      ~p", [Reason]),
	    ns_printlog(filename:join([PrivDir,ZoneDir,?LOG_FILE])),
	    throw(Reason);
	"Error: "++Error ->
            ?CL("ns_start -> error: "
               "~n      ~p", [Error]),
	    ns_printlog(filename:join([PrivDir,ZoneDir,?LOG_FILE])),
	    error(Error);
	_X ->
            ?CL("ns_start -> retry"),
	    ns_start(ZoneDir, PrivDir, NS, P)
    end.

ns_end(undefined, _PrivDir) -> undefined;
ns_end({ZoneDir,_NS,P}, PrivDir) when is_port(P) ->
    port_command(P, ["quit",io_lib:nl()]),
    ns_stop(P),
    ns_printlog(filename:join([PrivDir,ZoneDir,"ns.log"])),
    ok;
ns_end({Tag,_NS,P}, _PrivDir) when is_pid(P) ->
    Mref = erlang:monitor(process, P),
    P ! Tag,
    receive
        {'DOWN',Mref,_,_,Reason} ->
            Reason = normal,
            ok
    end.

ns_stop(P) ->
    case ns_collect(P) of
	eof ->
	    erlang:port_close(P);
	_ ->
	    ns_stop(P)
    end.

ns_collect(P) ->
    ns_collect(P, []).
ns_collect(P, Buf) ->
    receive
	{P,{data,{eol,L}}} ->
	    Line = lists:flatten(lists:reverse(Buf, [L])),
	    ?CL("collected: ~s", [Line]),
	    Line;
	{P,{data,{noeol,L}}} ->
	    ns_collect(P, [L|Buf]);
	{P,eof} ->
	    eof
    end.

ns_printlog(Fname) ->
    ?CL("Name server log file contents:"),
    case file:read_file(Fname) of
	{ok,Bin} ->
	    io:format("~s~n", [Bin]);
	_ ->
	    ok
    end.

zone_dir(_TC) ->
    otptest.
    % case TC of
	% basic                -> otptest;
	% name_addr_and_cached -> otptest;
	% resolve              -> otptest;
	% edns0                -> otptest;
	% edns0_multi_formerr  -> otptest;
	% files_monitor        -> otptest;
	% nxdomain_reply       -> otptest;
	% last_ms_answer       -> otptest;
	% update               -> otptest;
	% tsig_client          -> otptest;
    %     intermediate_error   ->
    %         {internal,
    %          #{rcode => ?REFUSED}};
    %     servfail_retry_timeout_default ->
    %         {internal,
    %          #{rcode => ?SERVFAIL, etd => 1500}};
    %     servfail_retry_timeout_1000 ->
    %         {internal,
    %          #{rcode => ?SERVFAIL, etd => 1000}};
	% _ -> undefined
    % end.

test(Config) ->
    ?CL("begin"),
    NS = ns(Config),
    NSs = [NS],
    Options = [{nameservers,NSs},verbose],
    Name = "ns.otptest",
    ct:pal("NS: ~p~n", [NS]),
    % Name = "ns.otptest",
    % NameC = caseflip(Name),
    % NameD = NameC ++ ".",
    % IP1 = {127,0,0,253},
    % IP2 = {127,0,0,254},
    % %%
    % %% nslookup
    {ok,Msg1} = inet_res:lookup(Name, in, naptr, [verbose]),
    % ?CL("nslookup with ~p: ~n      ~p", [Name, Msg1]),
    % [RR1, RR2] = lists:sort(inet_dns:msg(Msg1, anlist)),
    % IP1 = inet_dns:rr(RR1, data),
    % IP2 = inet_dns:rr(RR2, data),
    % Bin1 = inet_dns:encode(Msg1),
    % %%io:format("Bin1 = ~w~n", [Bin1]),
    % {ok,Msg1} = inet_dns:decode(Bin1),
    % %% Now with scrambled case
    % {ok,Msg1b} = inet_res:nslookup(NameC, in, a, NSs),
    % ?CL("nslookup with ~p: ~n      ~p", [NameC, Msg1b]),
    % [RR1b, RR2b] = lists:sort(inet_dns:msg(Msg1b, anlist)),
    % IP1 = inet_dns:rr(RR1b, data),
    % IP2 = inet_dns:rr(RR2b, data),
    % Bin1b = inet_dns:encode(Msg1b),
    % %%io:format("Bin1b = ~w~n", [Bin1b]),
    % {ok,Msg1b} = inet_dns:decode(Bin1b),
    % true =
	% (tolower(inet_dns:rr(RR1, domain))
	%  =:= tolower(inet_dns:rr(RR1b, domain))),
    % true =
	% (tolower(inet_dns:rr(RR2, domain))
	%  =:= tolower(inet_dns:rr(RR2b, domain))),
    % %%
    % %% resolve
    % {ok,Msg2} = inet_res:resolve(Name, in, a, Options),
    % ?CL("resolve with ~p: ~n      ~p", [Name, Msg2]),
    % [RR1c, RR2c] = lists:sort(inet_dns:msg(Msg2, anlist)),
    % IP1 = inet_dns:rr(RR1c, data),
    % IP2 = inet_dns:rr(RR2c, data),
    % Bin2 = inet_dns:encode(Msg2),
    % %%io:format("Bin2 = ~w~n", [Bin2]),
    % {ok,Msg2} = inet_dns:decode(Bin2),
    % %% Now with scrambled case
    % {ok,Msg2b} = inet_res:resolve(NameC, in, a, Options),
    % ?CL("resolve with ~p: ~n      ~p", [NameC, Msg2b]),
    % [RR1d, RR2d] = lists:sort(inet_dns:msg(Msg2b, anlist)),
    % IP1 = inet_dns:rr(RR1d, data),
    % IP2 = inet_dns:rr(RR2d, data),
    % Bin2b = inet_dns:encode(Msg2b),
    % %%io:format("Bin2b = ~w~n", [Bin2b]),
    % {ok,Msg2b} = inet_dns:decode(Bin2b),
    % true =
	% (tolower(inet_dns:rr(RR1c, domain))
	%   =:= tolower(inet_dns:rr(RR1d, domain))),
    % true =
	% (tolower(inet_dns:rr(RR2c, domain))
	%   =:= tolower(inet_dns:rr(RR2d, domain))),
    % ?CL("resolve \"127.0.0.1\"~n", []),
    % {ok, Msg3} =
    %     inet_res:resolve("127.0.0.1", in, a, Options),
    % [] = inet_dns:msg(Msg3, anlist),
    % {ok, Msg4} =
    %     inet_res:resolve("127.0.0.1", in, ptr, Options),
    % [RR4] = inet_dns:msg(Msg4, anlist),
    % "1.0.0.127.in-addr.arpa" = inet_dns:rr(RR4, domain),
    % "test1-78901234567890123456789012345678.otptest" =
    %     inet_dns:rr(RR4, data),
    % %%
    % %% lookup
    % ?CL("lookup"),
    % [IP1, IP2] =
    %     lists:sort(
    %       inet_res:lookup(Name, in, a, Options)),
    % [IP1, IP2] =
    %     lists:sort(
    %       inet_res:lookup(NameC, in, a, Options)),
    % [IP1, IP2] =
    %     lists:sort(
    %       inet_res:lookup(NameD, in, a, Options)),
    % %%
    % %% gethostbyname
    % ?CL("gethostbyname"),
    % {ok,#hostent{h_addr_list=IPs1}} =
    %     inet_res:gethostbyname(Name, inet, Options, infinity),
    % [IP1, IP2] = lists:sort(IPs1),
    % {ok,#hostent{h_addr_list=IPs2}} =
    %     inet_res:gethostbyname(NameC, inet, Options, infinity),
    % [IP1, IP2] = lists:sort(IPs2),
    % %%
    % %% getbyname
    % ?CL("getbyname"),
    % {ok,#hostent{h_addr_list=IPs3}} =
    %     inet_res:getbyname(Name, a, Options, infinity),
    % [IP1, IP2] = lists:sort(IPs3),
    % {ok,#hostent{h_addr_list=IPs4}} =
    %     inet_res:getbyname(NameC, a, Options, infinity),
    % [IP1, IP2] = lists:sort(IPs4),
    % ?CL("end"),
    % %%
    % %% gethostbyaddr
    % ?CL("gethostbyaddr"),
    % {ok,#hostent{h_name=Name,
    %              h_addr_list=[IP2]}} =
    %     inet_res:gethostbyaddr(IP2, Options, infinity),
    % {ok,#hostent{h_name=Name,
    %              h_addr_list=[IP1]}} =
    %     inet_res:gethostbyaddr(IP1, Options, infinity),
    ok.

%% Case flip helper, randomly flips the case of about every second [a-zA-Z]

-compile({inline, [caseflip/3]}).

caseflip([C | Cs]) when is_integer(C), $a =< C, C =< $z ->
    caseflip(Cs, C, $a - $A);
caseflip([C | Cs]) when is_integer(C), $A =< C, C =< $Z ->
    caseflip(Cs, C, $A - $a);
caseflip([C | Cs]) ->
    [C | caseflip(Cs)];
caseflip([]) ->
    [].
%%
caseflip(Cs, C, Diff) ->
    [case 0.5 =< rand:uniform() of
         true ->
             C - Diff;
         false ->
             C
     end | caseflip(Cs)].
