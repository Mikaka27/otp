-module(mr).
-compile([export_all, nowarn_export_all]).

-record(some_rec, {some_id :: number(), some_int :: number()}).

-define(NUM_RECORDS, 20000).

run(NumWorkers, NumTablesPerWorker, Retries) ->
    process_flag(trap_exit, true),
    PeersAndNodes = start_peers(NumWorkers),
    {Peers, Nodes} = lists:unzip(PeersAndNodes),
    ok = start_mnesia(Nodes),
    io:fwrite("Starting peers...~n"),
    ok = mnesia:start([{extra_db_nodes, Nodes}]),
    io:fwrite("Creating tables...~n"),
    ok = create_tables(Nodes, NumTablesPerWorker),
    ok = loop(PeersAndNodes, NumTablesPerWorker, Retries),
    ok = stop_mnesia(Nodes),
    stopped = mnesia:stop().

start_peers(NumWorkers) ->
    lists:map(fun(N) ->
        {ok, Controller, Node} = peer:start_link(#{
            name => "worker_" ++ integer_to_list(N),
            args => ["+S4"]
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

get_table_names(Node, NumTables) ->
    NodeNumber = get_node_number(Node),
    lists:map(fun(N) -> list_to_atom("table_" ++ NodeNumber ++ "_" ++ integer_to_list(N)) end,
              lists:seq(1, NumTables)).

create_tables(Nodes, NumTablesPerWorker) ->
    lists:foldl(fun(Node, Acc) ->
        TableNames = get_table_names(Node, NumTablesPerWorker),
        lists:foreach(fun(Table) ->
            io:fwrite("Creating table ~p...~n", [Table]),
            {atomic, ok} = mnesia:create_table(Table, [
                {type, set},
                {record_name, some_rec},
                {attributes, record_info(fields, some_rec)},
                {ram_copies, Acc ++ [Node]}
            ]),
            io:fwrite("Filling data in table ~p...~n", [Table]),
            [mnesia:dirty_write(Table, #some_rec{some_id = Id, some_int = Id}) || Id <- lists:seq(1, ?NUM_RECORDS)],
            io:fwrite("Table ~p created and filled with data.~n", [Table])
        end, TableNames),
        Acc ++ [Node]
    end, [], Nodes),
    ok.

verify_data_in_tables(Node, NumTablesPerWorker) ->
    TableNames = get_table_names(Node, NumTablesPerWorker),
    lists:foreach(fun(Table) ->
        Data = mnesia:dirty_match_object(Table, #some_rec{_ = '_'}),
        ExpectedData = lists:map(fun(Id) -> #some_rec{some_id = Id, some_int = Id} end,
                                 lists:seq(1, ?NUM_RECORDS)),
        case lists:sort(Data) of
            ExpectedData ->
                ok;
            _ ->
                {error, {unexpected_data, Data}}
        end
    end, TableNames).

remove_and_readd_node(Node, NumTablesPerWorker, PeersAndNodes) ->
    {_, AllNodes} = lists:unzip(PeersAndNodes),
    OtherNodes = lists:delete(Node, AllNodes),
    StartTime = erlang:monotonic_time(millisecond),
    Tables = mnesia:system_info(tables),
    ok = rpc:call(Node, mnesia, lkill, []),
    io:fwrite("Removing node ~p...~n", [Node]),
    Functions = lists:foldl(fun(schema, Acc) ->
                        Acc;
                    (Table, Acc) ->
                        Nodes = mnesia:table_info(Table, ram_copies),
                        io:fwrite("Checking table ~p with copies on nodes: ~p...~n", [Table, length(Nodes)]),
                        case lists:member(Node, Nodes) of
                            true when length(Nodes) == 1 ->
                                Acc;
                            true ->
                                io:fwrite("Removing table copy ~p from node ~p...~n", [Table, Node]),
                                {atomic, ok} = mnesia:del_table_copy(Table, Node),
                                [fun() ->
                                    io:fwrite("Re-adding table copy ~p on node ~p...~n", [Table, Node]),
                                    {atomic, ok} = mnesia:add_table_copy(Table, Node, ram_copies)
                                end | Acc];
                            false ->
                                Acc
                        end
    end, [], Tables),
    {Peer, Node} = lists:keyfind(Node, 2, PeersAndNodes),
    true = exit(Peer, kill),
    WorkerName = "worker_" ++ get_node_number(Node),
    {ok, NewPeer, NewNode} = peer:start_link(#{
        name => WorkerName,
        args => ["+S4"]
    }),
    io:fwrite("Restarting node ~p...~n", [NewNode]),
    ok = rpc:call(NewNode, mnesia, start, [[{extra_db_nodes, OtherNodes}]]),
    [Fun() || Fun <- Functions],
    try
        io:fwrite("Waiting for tables to be ready on node ~p...~n", [NewNode]),
        ok = rpc:call(NewNode, mnesia, wait_for_tables, [get_table_names(NewNode, NumTablesPerWorker), 120000]),
        io:fwrite("Verifying data in tables on node ~p...~n", [NewNode]),
        ok = rpc:call(NewNode, ?MODULE, verify_data_in_tables, [NewNode, NumTablesPerWorker]),
        io:fwrite("Restart took: ~p~n", [erlang:monotonic_time(millisecond) - StartTime])
    catch
        C : R : ST ->
            io:fwrite("Error during re-adding node: ~p ~p:~p:~p~n", [Node, C, R, ST]),
            mnesia_lib:dist_coredump(),
            erlang:raise(C, R, ST)
    end,
    lists:keyreplace(Peer, 1, PeersAndNodes, {NewPeer, NewNode}).

loop(_, _, 0) ->
    ok;
loop(PeersAndNodes, NumTablesPerWorker, Retries) ->
    {Peers, Nodes} = lists:unzip(PeersAndNodes),
    Index = rand:uniform(length(Nodes) - 1),
    %% Ensure we don't pick the first node, it's table has copy only on node 1
    Node = lists:nth(Index + 1, Nodes),
    NewPeersAndNodes = remove_and_readd_node(Node, NumTablesPerWorker, PeersAndNodes),
    loop(NewPeersAndNodes, NumTablesPerWorker, Retries - 1).

