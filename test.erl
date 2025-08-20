%%%-------------------------------------------------------------------
%%% File    : test.erl
%%% Author  : Dan Gudmundsson <dgud@erlang.org>
%%% Description :  Simulate problem in ERIERL-976
%%%      (where controller nodes restart with new name)
%%%
%%%      ERL_AFLAGS=+S4
%%%      ERL_FLAGS="-pa /home/dgud/mnesia_test/big_restart"
%%% Created : 21 Jan 2010 by Dan Gudmundsson <dgud@erlang.org>
%%% Parameters for ERIERL-1258
%%% test:go(32, 1000, 10) - problem starts if it takes longer than 2 minutes
%%%-------------------------------------------------------------------
-module(test).
-compile([export_all, nowarn_export_all]).

ns() ->
    %%[a@armed,b@armed,c@armed,d@armed,e@armed,f@armed,g@armed].
    List = [a,b,c,d, e,f,g,i,
	    k,l,m,n, o,p,q,r],
    ['t100@localhost', 's100@localhost' |
     [list_to_atom(atom_to_list(Name1) ++ atom_to_list(Name2) ++ "@localhost")
      || Name1 <- List, Name2 <- List]].


newNode(OldNode) ->
    [[C|IntString], "localhost"] = string:lexemes(atom_to_list(OldNode), "@"),
    {Int, []} = string:to_integer(IntString),
    list_to_atom([C| integer_to_list(Int+1)] ++ "@localhost").

go(NoNodes, NoTabs, NoTests) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    mnesia:kill(),
    {Ns,_} = lists:split(NoNodes, ns()),
    [H1,H2|Other] = Ns,
    case hd(Other) == node() of
	false ->
	    io:format("Should be runned from ~p and not ~p~n", [hd(Other), node()]),
            exit(H1);
	true ->
            ok
    end,

    Main = [H1,H2],
    start_nodes([H1, H2]),

    ok = rpc:call(H1, mnesia, start, []),
    Tabs1 = [list_to_atom("all" ++ integer_to_list(I)) || I <- lists:seq(1, NoTabs div 2)],
    Tabs2 = [list_to_atom("main" ++ integer_to_list(I)) || I <- lists:seq(1, NoTabs div 2)],

    [{atomic, ok} = rpc:call(H1, mnesia, create_table, [Tab, [{ram_copies, ns()}]]) || Tab <- Tabs1],
    [{atomic, ok} = rpc:call(H1, mnesia, create_table, [Tab, [{ram_copies, Main}]]) || Tab <- Tabs2],

    io:format("~n~n Fill tables ~n~n",[]),
    [fill_table(H1, list_to_atom("all" ++ integer_to_list(I))) || I <- lists:seq(1, NoTabs div 2), (I rem 10) == 0],
    [fill_table(H1, list_to_atom("main" ++ integer_to_list(I))) || I <- lists:seq(1, NoTabs div 2), (I rem 2) == 0],

    Tabs = Tabs2 ++ Tabs1,

    ok = rpc:call(H2, mnesia, start, [[{extra_db_nodes, Ns}, {no_table_loaders, 100}]]),
    io:format("~n~n Start nodes ~n~n",[]),
    start_nodes(Other),
    io:format("~n~n Starting mnesia ~n~n",[]),

    %% Start all other nodes
    [spawn(Node, ?MODULE, start_mnesia, [self(), Main]) || Node <- Other],
    rec_results(Other,[]),

    io:format("~p ~p: ~p~n",[H1, main2, rpc:call(H1, mnesia, table_info, [main2, size])]),
    io:format("~p ~p: ~p~n",[H2, main2, rpc:call(H2, mnesia, table_info, [main2, size])]),

    Stats = {0, 0, 0, 999999999999999, -1},

    % io:format("~n~n STARTING TEST plain~n~n",[]),
    %% R1 = loop(NoTests, Tabs, Main, Other, plain, Stats),

    %% io:format("~n~n STARTING TEST orig~n~n",[]),
    %% [[N1,N2|Rest]|_] = R2 = loop(NoTests, Tabs, Main, Other, orig, Stats),

    %% %% io:format("~n~n STARTING TEST schema~n~n",[]),
    %% %% [[N1,N2|Rest]|_] = R4 = loop(NoTests, Tabs, Main, Other, schema, Stats),

    %% io:format("~n~n STARTING TEST before~n~n",[]),
    %% R3 = loop(NoTests, Tabs, [N1,N2], Rest, before, Stats),

    io:format("~n~n STARTING TEST init~n~n",[]),
    R1 = loop(NoTests, Tabs, Main, Other, init, Stats),

    apply(?MODULE, print_stats, R1),
    %% apply(?MODULE, print_stats, R2),
    %% apply(?MODULE, print_stats, R3),
    %% apply(?MODULE, print_stats, R4),
    ok.


loop(N, Tabs, Main, Ns, Variant, Stats0) when N > 0 ->
    %% io:format("**********************************************************"),
    %% io:format("Starting test ~p~n",[N]),

    Me = self(),

    Restart = case Variant of
                  orig -> fun orig_restart/4;
                  plain -> fun plain_restart/4;
                  before -> fun before_restart/4;
                  schema -> fun schema_restart/4;
                  init -> fun init_restart/4
              end,

    %% timer:send_after(20_000, restart_next),

    Test = fun() ->
                   [N1, N2] = Main,
                   _Pid1 = spawn_link(fun() -> Restart(N1, N2, Ns, Me) end),
                   receive
                       {_, NewN1} = Res -> self() ! Res
                   end,
                   _Pid2 = spawn_link(fun() -> Restart(N2, NewN1, Ns, Me) end),
                   lists:reverse(rec_results(Main, []))
           end,

    receive restart_next -> ok
    after 500 -> ok
    end,

    {Time, NewMain} = timer:tc(Test),
    %% io:format("~p Time ~pms~n", [N, Time div 1000]),
    Stats = calc_stats(Time, Stats0),
    timer:sleep(1000),
    loop(N-1, Tabs, NewMain, Ns, Variant, Stats);
loop(_, Tabs, Main, Ns, Variant, Stats) ->
    [Main++Ns, Tabs, Variant, Stats].


%% plain:    31000 ms
%% orig:    217780 ms
%% before:         ms

plain_restart(OldNode, _Other, Ns, Pid) ->
    try
        io:format("~p ~p: ~p~n",[OldNode, main2, rpc:call(OldNode, mnesia, table_info, [main2, size])]),

        slave:stop(OldNode),
        start_nodes([OldNode]),
        ok = rpc:call(OldNode, mnesia, start, [[{extra_db_nodes, Ns}, %% {debug, verbose},
                                                {no_table_loaders, 100}]]),
        io:format("~p: ~p ~0.P~n", [OldNode, time(),
                                    rpc:call(OldNode, mnesia_controller,
                                         get_info, [1000]), 10]),
        erpc:call(OldNode, fun() -> wait_for_tables(foo) end),
        io:format("~p ~p: ~p~n",[OldNode, main2, rpc:call(OldNode, mnesia, table_info, [main2, size])]),

        io:format("Done: ~p ~p~n", [OldNode, time()]),
        Pid ! {OldNode, OldNode}

    catch
        _:Reason:ST ->
            io:format("CRASH ~p   ~p~n ~p~n", [OldNode, Reason, ST]),
            exit(crash)
    end.

orig_restart(OldNode, Other, Ns, Pid) ->
    NewNode = newNode(OldNode),
    io:format("~p -> ~p~n",[OldNode, Other]),
    MyTabs = rpc:call(OldNode, mnesia, system_info, [local_tables]) -- [schema],
    try
        slave:stop(OldNode),
        start_nodes([NewNode]),
        ok = rpc:call(NewNode, mnesia, start, [[{extra_db_nodes, Ns}, %% {debug, verbose},
                                                {no_table_loaders, 100}]]),
        io:format("~p: ~p ~0.P~n", [NewNode, time(),
                                  rpc:call(NewNode, mnesia_controller,
                                           get_info, [1000]), 20]),
        [{atomic, ok} = erpc:call(Other, fun() -> addTable(Tab, NewNode) end) || Tab <- MyTabs],
        {atomic, ok} = rpc:call(NewNode, mnesia, del_table_copy, [schema, OldNode]),
        io:format("Done: ~p ~p~n", [NewNode, time()]),
        Pid ! {OldNode, NewNode}
    catch
        _:Reason:ST ->
            io:format("CRASH ~p -> ~p   ~p~n ~p~n", [OldNode, NewNode, Reason, ST]),
            exit(crash)
    end.

before_restart(OldNode, Other, Ns, Pid) ->
    NewNode = newNode(OldNode),
    io:format("~p -> ~p~n",[OldNode, Other]),
    MyTabs = rpc:call(OldNode, mnesia, system_info, [local_tables]) -- [schema],
    try
        slave:stop(OldNode),
        start_nodes([NewNode]),

        [{atomic, ok} = erpc:call(Other, fun() -> addTable(Tab, NewNode) end) || Tab <- MyTabs],

        ok = rpc:call(NewNode, mnesia, start, [[{extra_db_nodes, Ns}, %% {debug, verbose},
                                                {no_table_loaders, 100}]]),
        io:format("~p: ~p ~0.P~n", [NewNode, time(),
                                    rpc:call(NewNode, mnesia_controller,
                                             get_info, [1000]), 20]),
        erpc:call(NewNode, fun() -> wait_for_tables(foo) end),
        {atomic, ok} = rpc:call(NewNode, mnesia, del_table_copy, [schema, OldNode]),
        io:format("Done: ~p ~p~n", [NewNode, time()]),
        Pid ! {OldNode, NewNode}
    catch
        _:Reason:ST ->
            io:format("CRASH ~p -> ~p   ~p~n ~p~n", [OldNode, NewNode, Reason, ST]),
            exit(crash)
    end.

schema_restart(OldNode, Other, Ns, Pid) ->
    NewNode = newNode(OldNode),
    io:format("~p -> ~p~n",[OldNode, Other]),
    MyTabs = rpc:call(OldNode, mnesia, system_info, [local_tables]) -- [schema],
    try
        slave:stop(OldNode),
        start_nodes([NewNode]),

        {atomic, ok} = erpc:call(Other, fun() -> add_tables(MyTabs, NewNode) end),

        ok = rpc:call(NewNode, mnesia, start, [[{extra_db_nodes, Ns}, %% {debug, verbose},
                                                {no_table_loaders, 100}]]),
        io:format("~p: ~p ~0.P~n", [NewNode, time(),
                                    rpc:call(NewNode, mnesia_controller,
                                             get_info, [1000]), 20]),
        erpc:call(NewNode, fun() -> wait_for_tables(foo) end),
        {atomic, ok} = rpc:call(NewNode, mnesia, del_table_copy, [schema, OldNode]),
        io:format("Done: ~p ~p~n", [NewNode, time()]),
        Pid ! {OldNode, NewNode}
    catch
        _:Reason:ST ->
            io:format("CRASH ~p -> ~p   ~p~n ~p~n", [OldNode, NewNode, Reason, ST]),
            exit(crash)
    end.

add_tables(Tables, NewNode) ->
    %% Needs mnesia hacks  exported mnesia_schema:do_add_table_copy
    AddTable = fun(Tab) -> mnesia_schema:do_add_table_copy(Tab, NewNode, ram_copies) end,
    AddTables = fun() ->
                        case lists:member(NewNode, mnesia:system_info(db_nodes)) of
                            true -> mnesia:abort({already_alive, NewNode});
                            false -> lists:foreach(AddTable, Tables)
                        end
                end,
    mnesia_schema:schema_transaction(AddTables).


init_restart(OldNode, Other, Ns, Pid) ->
    io:format("~p -> ~p~n",[OldNode, Other]),
    MyTabs = rpc:call(OldNode, mnesia, system_info, [local_tables]) -- [schema],
    try
        slave:stop(OldNode),
        start_nodes([OldNode]),

        ReqId = erpc:send_request(Other, fun() -> delTables(MyTabs, OldNode) end),
        ok = rpc:call(OldNode, mnesia, start, [[{extra_db_nodes, Ns}, %% {debug, verbose},
                                                {no_table_loaders, 100}]]),
        io:format("~p: ~p ~0.P~n", [OldNode, time(),
                                    rpc:call(OldNode, mnesia_controller,
                                             get_info, [1000]), 20]),
        {response, []} = erpc:wait_response(ReqId, infinity),
        [{atomic, ok} = erpc:call(Other, fun() -> addTable(Tab, OldNode) end) || Tab <- MyTabs],
        io:format("Done: ~p ~p~n", [OldNode, time()]),
        Pid ! {Other, OldNode}
    catch
        _:Reason:ST ->
            io:format("CRASH ~p -> ~p   ~p~n ~p~n", [OldNode, Other, Reason, ST]),
            exit(crash)
    end.


addTable(Tab) ->
    addTable(Tab, node()).
addTable(Tab, Node) ->
    case mnesia:add_table_copy(Tab, Node, ram_copies) of
        {aborted, {system_limit, Tab, _}} ->
            io:format("Retry ~p ~p~n", [Node, Tab]),
            mnesia:add_table_copy(Tab, Node, ram_copies);
        Res ->
            Res
    end.

delTables([Tab|Tabs], Node) ->
    case mnesia:del_table_copy(Tab, Node) of
        {atomic, ok} ->
            delTables(Tabs, Node);
        {aborted, Err} ->
            [Err|delTables(Tabs, Node)]
    end;
delTables([], _) ->
    [].

calc_stats(Diff, {N, Sum, SumSq, Min, Max}=_S0) ->
    case _ = N rem 5 of
	%% 0 when N > 1 ->
	%%     print_stats(_S0),
	%%     {N+1, Sum+Diff, SumSq+Diff*Diff, min(Diff, Min), max(Diff, Max)};
	_ ->
	    {N+1, Sum+Diff, SumSq+Diff*Diff, min(Diff, Min), max(Diff, Max)}
    end.

print_stats(Ns, Tabs, Variant, Stats) ->
    io:format("~n~p; ~p; ~p; ",[Variant, length(Ns), length(Tabs)]),
    print_stats(Stats).

print_stats({N, Sum, SumSq, Min, Max}) ->
    Mean = Sum / N,
    StdDev = math:sqrt((SumSq - (Sum*Sum/N))/max(1,(N - 1))),
    io:format("~n time: ~.1f; ~.3f;\t\t~w; ~w; ~w;~n~n",
	      [Mean / 1000, StdDev / 1000, Min div 1000, Max div 1000, N]).


rec_results([Node|Ns],Acc) ->
    receive
	{Node,Res} ->
	    %% io:format("RES ~p ~p~n", [Node,_Res]),
	    rec_results(Ns,[Res|Acc])
    end;
rec_results([],Rem) ->
    %% io:format("**********************************************************~n"),
    Rem.

start_mnesia(P, Main) ->
    mnesia:start([{extra_db_nodes, Main}, {core_dir,"."}]),
    %% io:format("~p STARTED waiting on ~p tables ~n", [node(), length(Local)]),
    Res = wait_for_tables(P),
    P ! {node(),Res}.

wait_for_tables(P) ->
    Local = mnesia_lib:val({schema, local_tables}),
    %% io:format(" ~p WAITING ~p ~n", [node(), length(Local)]),
    case mnesia:wait_for_tables(Local, 10000) of
	{timeout, _TMO} ->
	    io:format(" ~p STILL WAITING ~p ~n", [node(), length(_TMO)]),
	    wait_for_tables(P);
	ok ->
	    case mnesia_controller:get_workers(infinity) of
		{workers,A, _,_} when A == undefined; A == []  ->
		    {loaded, length(Local)};
		_Left ->
		    %% io:format("~p WAITING worker ~p ~n", [node(), Left]),
		    wait_for_tables(P)
	    end
    end.

start_nodes(Ns) ->
    [spawn(?MODULE, slave_start, [Node,self()]) || Node <- Ns],
    rec_res(Ns, []).

rec_res([], Ok) -> Ok;
rec_res([_|Ns], Ok) ->
    receive
	{ok,N}  -> rec_res(Ns, [N|Ok]);
	{error,_N} -> rec_res(Ns, Ok)
    end.

slave_start(Node, Father) ->
    [Name,Host] =  string:tokens(atom_to_list(Node),"@"),
    case slave_start2(Host,Name, 0) of
	{ok, Node} = Res->
	    rpc:call(Node, error_logger, delete_report_handler, [error_logger_tty_h]),
	    Father ! Res;
	Res ->
	    Father ! Res
    end.

slave_start2(Host, N, C) when C < 5->
    case slave:start(Host, N) of
	{ok, Node} ->
	    %% io:format("s"),
	    {ok, Node};
	{error,{already_running,Node}} ->
	    {ok, Node}; %%slave_start(N,C+1);
	Else ->
	    io:format("Error ~p~n", [Else]),
	    {error, N}
    end.

fill_table(Node, Tab) ->
    AList = lists:seq(1,200),
    MakeData = fun() -> _ = [mnesia:dirty_write({Tab, Key, AList}) || Key <- lists:seq(1, 4000)] end,
    erpc:call(Node, MakeData).
