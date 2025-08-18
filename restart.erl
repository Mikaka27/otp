-module(restart).

-compile(export_all).

ns() ->
    %%[a@armed,b@armed,c@armed,d@armed,e@armed,f@armed,g@armed].
    [_,Host] =  string:tokens(atom_to_list(node()),"@"),
    List = [a,b,c,d], %% , e,f,g,i, k,l,m,n, o,p,q,r],
    [list_to_atom(atom_to_list(Name) ++ "@" ++ Host) || Name <- List].

go(N) ->
    %% mnesia:kill(),
    [H1,H2|Other] = ns(),
    case node() of
	H1 -> ok;
	Other ->
	    io:format("Should be runned from ~p and not ~p~n", [node(), H1])
    end,

    Main = [H1,H2],
    start_nodes([H2]),

    Tabs = [list_to_atom("tab" ++ integer_to_list(I)) || I <- lists:seq(1, 200)],
    try mnesia:table_info(hd(Tabs), where_to_read) of
        H1 -> ok;
        Hmm -> exit({error, node(), Hmm})
    catch _:_ ->
            ok = mnesia:start([]),
            [{atomic, ok} = mnesia:create_table(Tab, [{ram_copies, ns()}])
             || Tab <- Tabs],

            io:format("~n~n Fill tables ~n~n",[]),
            [fill_table(H1, Tab) || Tab <- Tabs]
    end,

    ok = rpc:call(H2, mnesia, start, [[{extra_db_nodes, [H1]}, {no_table_loaders, 10}]]),
    start_nodes(Other),
    ok = rpc:call(H2, mnesia, wait_for_tables, [Tabs, 40000]),

    Res = loop(N, Tabs, Main, Other),
    Res.

loop(N, Tabs, Main, Ns) when N > 0 ->
    io:format("**********************************************************~n"),
    io:format("Starting test ~p~n",[N]),
    %% Pids = [spawn(Node, ?MODULE, test, [self(), Main, Ns]) || Node <- Ns],
    Self = self(),
    TestNode = hd(Ns),
    Tracer = spawn_link(TestNode, fun() -> start_tracing(Self) end),

    Pid1 = spawn_link(fun() ->
                              RTabs = randomize(Tabs),
                              D = fun() ->
                                          [{Tab, mnesia:del_table_copy(Tab, TestNode)}
                                           || Tab <- RTabs]
                                  end,
                              {Time, Res} = timer:tc(D),
                              io:format("Del Tabs: ~.1fms ~n", [Time / 1000]),
                              Self ! {self(), delete, Res}
                      end),
    timer:sleep(70),
    ok = erpc:call(TestNode, mnesia, start, [[{extra_db_nodes, Main}, {core_dir,"."},
                                              {debug, debug},
                                              {no_table_loaders, 10}]]),
    timer:sleep(50),
    Pid2 = spawn_link(fun() ->
                              Res = [{Tab, mnesia:add_table_copy(Tab, TestNode, ram_copies)}
                                     || Tab <- randomize(Tabs)],
                              Self ! {self(), added, Res}
                      end),

    case rec_results([Pid1,Pid2], 40_000, timeout) of
	true ->
            Loaded = lists:sort(erpc:call(TestNode, mnesia, system_info, [local_tables])),
            %% io:format("Not loaded: ~p~n", [Tabs -- Loaded]),
            WaitRes = erpc:call(TestNode, fun() -> mnesia:wait_for_tables(Tabs, 2000) end),
            io:format("Wait: ~p~n", [WaitRes]),
            stopped = erpc:call(TestNode, mnesia, stop, []),
            timer:sleep(100),
            Tracer ! stop_trace,
	    loop(N-1, Tabs, Main, Ns);
        Res ->
	    io:format("Stopping due to error ~p ~p~n",[Tracer, Res])
    end;
loop(_,_,_,_) -> ok.

randomize(List) ->
    [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- List])].

rec_results(Pids, Timeout, Result) ->
    R = fun(R) when Result =:= timeout -> R;
           (R) when Result =:= true -> R;
           (R) -> [R|Result]
        end,
    receive
        {trace_fail, _Tab} = Msg ->
            rec_results(Pids, Timeout, R(Msg));
        {_Pid, delete, Deleted} ->
            Bad = lists:filter(fun({_, {atomic, ok}}) -> false;
                                  ({_, {aborted, {badarg, _, unknown}}}) -> false;
                                  (_) -> true
                               end, Deleted),
            io:format("Bad delete ~P~n",[Bad, 20]),
            rec_results(Pids, Timeout, R(Bad =:= []));
        {_Pid, added, Added} ->
            Bad = lists:filter(fun({_, {atomic, ok}}) -> false;
                                  ({_, {aborted, {already_exists, _, _}}}) -> false;
                                  (_) -> true
                               end, Added),
            io:format("Bad add ~P~n",[Bad, 20]),
            rec_results(Pids, 500, R(Bad =:= []))
    after Timeout ->
            Result
    end.

start_tracing(Pid) ->
    io:format("Starting Trace:~n",[]),
    dbg:tracer(process, {fun restart:print_trace/2, #{tester => Pid}}),
    dbg:p(new, [call]),
    dbg:tpl({mnesia_schema, prepare_op, 3}, []),
    dbg:tpl({mnesia_dumper, insert_op, 5}, []),
    dbg:tpl({mnesia_loader, do_get_network_copy, 5}, x),
    dbg:tpl({mnesia_loader, init_receiver, 5}, x),

    receive
        stop_trace -> dbg:stop()
    end,
    ok.

print_trace({trace, _, call,
             {mnesia_schema, prepare_op, [_Tid, {op, add_table_copy, _Storage, _Node, [{name, Tab}|_]}, _]
             }}, Acc) ->
    %% io:format("Prepare Add: ~p~n",[Tab]),
    add(Tab, prepare_add, Acc);
print_trace({trace, _, call,
             {mnesia_schema, prepare_op, [_Tid, {op, del_table_copy, _Storage, _Node, [{name, Tab}|_]}, _]
             }}, Acc) ->
    %% io:format("Prepare Del: ~p~n",[Tab]),
    add(Tab, prepare_del, Acc);
print_trace({trace, _, call, {mnesia_schema, prepare_op, _}}, Acc) ->
    Acc;
print_trace({trace, _, call,
             {mnesia_dumper, insert_op, [_Tid, _, {op, del_table_copy, _Storage, _Node, [{name, Tab}|_]}| _]
             }}, Acc) ->
    %% io:format("Prepare Del: ~p~n",[Tab]),
    add(Tab, do_del, Acc);
print_trace({trace, _, call, {mnesia_dumper, insert_op, _}}, Acc) ->
    Acc;

print_trace({trace, Pid, call, {_, do_get_network_copy, [Tab, Reason, _, unknown, _]}}, Acc) ->
    %% io:format("Failed Fetch ~p  ~p~n",[Tab, Reason]),
    add(Tab, {fetch, Pid, Reason}, Acc#{Pid => Tab});
print_trace({trace, Pid, call, {_, do_get_network_copy, [Tab, Reason, _, _, _]}}, Acc) ->
    %% io:format("Fetch ~p  ~p~n",[Tab, Reason]),
    add(Tab, {fetch, Pid, Reason}, Acc#{Pid => Tab});
print_trace({trace, Pid, return_from, {_, do_get_network_copy, 5}, Result}, Acc) ->
    Tab = maps:get(Pid, Acc, unknown),
    %% io:format("Fetch result ~p  ~p~n",[Tab, Result]),
    case Result of
        {loaded, ok} ->
            Acc;
        _ when Tab =:= unknown ->
            io:format("~p ~p~n", [Tab, Result]),
            Acc;
        {not_loaded, storage_unknown} ->
            Ops = maps:get(Tab, Acc),
            io:format("Tab: ~p ~p fetch done ~p ~0p~n", [Tab, lists:reverse(Ops), Pid, Result]),
            Acc;
        _ ->
            Ops = maps:get(Tab, Acc),
            io:format("Tab: ~p ~p  fetch done ~p ~0p~n", [Tab, lists:reverse(Ops), Pid, Result]),
            maps:get(tester, Acc) ! {trace_fail, Tab},
            Acc
    end;
print_trace({trace, Pid, exception_from, {_, do_get_network_copy, 5}, Result}, Acc) ->
    Tab = maps:get(Pid, Acc, unknown),
    %% io:format("Fetch result ~p  ~p~n",[Tab, Result]),
    case Result of
        _ when Tab =:= unknown ->
            io:format("~p ~p~n", [Tab, Result]),
            Acc;
        _ ->
            Ops = maps:get(Tab, Acc),
            io:format("Tab: ~p ~p  fetch done ~p ~0p~n", [Tab, lists:reverse(Ops), Pid, Result]),
            maps:get(tester, Acc) ! {trace_fail, Tab},
            Acc
    end;

print_trace({trace, Pid, call, {_, init_receiver, [_, Tab|_]}}, Acc) ->
    add(Tab, {init_r, Pid}, Acc#{Pid => Tab});
print_trace({trace, Pid, return_from, {_, init_receiver, 5}, Result}, Acc) ->
    Tab = maps:get(Pid, Acc, unknown),
    add(Tab, {init_r_done, Pid, Result}, Acc);
print_trace({trace, Pid, exception_from, {_, init_receiver, 5}, Result}, Acc) ->
    Tab = maps:get(Pid, Acc, unknown),
    add(Tab, {init_r_done, Pid, Result}, Acc);

print_trace(Trace, Acc) ->
    io:format("Trace: ~p~n",[Trace]),
    Acc.

add(Tab, What, Acc) ->
    Prev = maps:get(Tab, Acc, []),
    Acc#{Tab => [What|Prev]}.


%% test(P, Main, Ns) ->
%%     %% {_,_,T} = erlang:now(),
%%     %% timer:sleep(200+ (T rem 200)),
%%     mnesia:start([{extra_db_nodes, Main}, {core_dir,"."}, {no_table_loaders, 10}]),
%%     Local = mnesia_lib:val({schema, local_tables}),
%%     io:format("~p STARTED with ~p tables ~n", [node(), length(Local)]),
%%     Res = wait_for_tables(P, lists:sort(Main++Ns), Local),
%%     P ! {node(),Res},
%%     case Res of 
%% 	[] -> 
%% 	    receive do_kill -> ok end,
%% 	    mnesia:stop(),
%% 	    %%P ! {self(), killed};
%% 	    timer:sleep(100);
%% 	_ ->
%% 	    leave_alive_for_post_debugging	   
%%     end.

%% wait_for_tables(P, Ns, Tables) ->
%%     ok = mnesia:wait_for_tables(Tables, 60000),
%%     P ! {self(), wait_ok},
%%     receive do_check -> ok end,    
%%     Check = fun(Tab, Error) ->
%% 		    %%TabNodes = [N || {N,_} <- mnesia_lib:val({Tab, where_to_commit})],
%% 		    TabNodes = mnesia_lib:val({Tab, where_to_write}),
%% 		    DbN = lists:sort(mnesia_lib:val({current, db_nodes})),
%% 		    case lists:sort(TabNodes) == Ns andalso {DbN == Ns, nodes} of
%% 			{true,_} -> 
%% 			    Error;
%% 			{false, _Foo} -> 
%% 			    [{nodes,Ns--TabNodes}|Error];
%% 			false -> 
%% 			    [{Tab,Ns--TabNodes}|Error]
%% 		    end
%% 	    end,
%%     lists:foldl(Check, [], Tables).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    Res = slave_start2(Host,Name, 0),
    Father ! Res.

slave_start2(Host, N, C) when C < 5->
    case slave:start(Host, N) of
	{ok, Node} ->
	    io:format("s"),
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
