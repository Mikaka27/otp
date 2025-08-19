-module(mr).
-compile([export_all, nowarn_export_all]).

-record(some_rec, {some_id :: number(), some_int :: number()}).

-define(NUM_RECORDS, 20000).

run() ->
    run(100).
run(Retries) ->
    PeersAndNodes = start_peers(10),
    {Peers, Nodes} = lists:unzip(PeersAndNodes),
    ok = start_mnesia(Nodes),
    ok = mnesia:start([{extra_db_nodes, Nodes}]),
    ok = create_tables(Nodes),
    ok = loop(PeersAndNodes, Retries),
    ok = stop_mnesia(Nodes),
    stopped = mnesia:stop().

start_peers(NumWorkers) ->
    lists:map(fun(N) ->
        {ok, Controller, Node} = peer:start(#{
            name => "worker_" ++ integer_to_list(N)
        }),
        {Controller, Node}
    end, lists:seq(1, NumWorkers)).

start_mnesia(Nodes) ->
    lists:foldl(fun(Node, Acc) ->
        ok = rpc:call(Node, mnesia, start, [[{extra_db_nodes, Acc}]]),
        [Node | Acc]
    end, [], Nodes),
    ok.

stop_mnesia(Nodes) ->
    lists:foreach(fun(Node) ->
        stopped = rpc:call(Node, mnesia, stop, [])
    end, Nodes),
    ok.

get_node_number(Node) ->
    NodeString = atom_to_list(Node),
    [NodeName | _] = string:split(NodeString, "@"),
    [_, NodeNumber] = string:split(NodeName, "_"),
    NodeNumber.

get_table_name(Node) ->
    NodeNumber = get_node_number(Node),
    list_to_atom("table_" ++ NodeNumber).

create_tables(Nodes) ->
    lists:foldl(fun(Node, Acc) ->
        Table = get_table_name(Node),
        {atomic, ok} = mnesia:create_table(Table, [
            {type, set},
            {record_name, some_rec},
            {attributes, record_info(fields, some_rec)},
            {ram_copies, Acc ++ [Node]}
        ]),
        ok = mnesia:sync_dirty(fun() ->
            lists:foreach(fun(Id) ->
                mnesia:write(Table, #some_rec{some_id = Id, some_int = Id}, write)
            end, lists:seq(1, ?NUM_RECORDS))
        end),
        Acc ++ [Node]
    end, [], Nodes),
    ok.

verify_data_in_table(Node) ->
    Table = get_table_name(Node),
    Data = mnesia:dirty_match_object(Table, #some_rec{_ = '_'}),
    ExpectedData = lists:map(fun(Id) -> #some_rec{some_id = Id, some_int = Id} end,
                             lists:seq(1, ?NUM_RECORDS)),
    case lists:sort(Data) of
        ExpectedData ->
            ok;
        _ ->
            {error, {unexpected_data, Data}}
    end.

remove_and_readd_node(Node, PeersAndNodes) ->
    Tables = mnesia:system_info(tables),
    ok = rpc:call(Node, mnesia, lkill, []),
    Functions = lists:foldl(fun(schema, Acc) ->
                        Acc;
                    (Table, Acc) ->
                        Nodes = mnesia:table_info(Table, ram_copies),
                        case lists:member(Node, Nodes) of
                            true when length(Nodes) == 1 ->
                                Acc;
                            true ->
                                {atomic, ok} = mnesia:del_table_copy(Table, Node),
                                [fun() -> {atomic, ok} = mnesia:add_table_copy(Table, Node, ram_copies) end | Acc];
                            false ->
                                Acc
                        end
    end, [], Tables),
    {Peer, Node} = lists:keyfind(Node, 2, PeersAndNodes),
    true = exit(Peer, kill),
    WorkerName = "worker_" ++ get_node_number(Node),
    {ok, NewPeer, NewNode} = peer:start(#{
        name => WorkerName
    }),
    {_, AllNodes} = lists:unzip(PeersAndNodes),
    OtherNodes = lists:delete(Node, AllNodes),
    ok = rpc:call(NewNode, mnesia, start, [[{extra_db_nodes, OtherNodes}]]),
    lists:foreach(fun(Fun) ->
        Fun()
    end, Functions),
    try
        ok = rpc:call(NewNode, mnesia, wait_for_tables, [[get_table_name(NewNode)], 120000]),
        ok = rpc:call(NewNode, ?MODULE, verify_data_in_table, [NewNode])
    catch
        C : R : ST ->
            io:fwrite("Error during re-adding node: ~p ~p:~p:~p~n", [Node, C, R, ST]),
            mnesia_lib:dist_coredump(),
            erlang:raise(C, R, ST)
    end,
    lists:keyreplace(Peer, 1, PeersAndNodes, {NewPeer, NewNode}).

loop(_, 0) ->
    ok;
loop(PeersAndNodes, Retries) ->
    {Peers, Nodes} = lists:unzip(PeersAndNodes),
    Index = random:uniform(length(Nodes) - 1),
    %% Ensure we don't pick the first node, it's table has copy only on node 1
    Node = lists:nth(Index + 1, Nodes),
    NewPeersAndNodes = remove_and_readd_node(Node, PeersAndNodes),
    loop(NewPeersAndNodes, Retries - 1).

