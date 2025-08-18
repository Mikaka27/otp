-module(mr).
-compile([export_all, nowarn_export_all]).

-record(some_rec, {some_id :: atom(), some_int :: number()}).

-define(NUM_RECORDS, 1000).

run() ->
    PeersAndNodes = start_peers(10),
    {Peers, Nodes} = lists:unzip(PeersAndNodes),
    ok = start_mnesia(Nodes),
    ok = mnesia:start([{extra_db_nodes, Nodes}]),
    ok = create_tables(Nodes),
    [Node | _] = Nodes,
    ok = verify_data_in_table(Node),
    remove_and_readd_node(Node, PeersAndNodes),
    timer:sleep(3000),
    stop_mnesia(Nodes),
    ok = mnesia:stop().

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
        ok = rpc:call(Node, mnesia, stop, [])
    end, Nodes),
    ok.

create_tables(Nodes) ->
    lists:foldl(fun(Node, Acc) ->
        {atomic, ok} = mnesia:create_table(Node, [
            {type, set},
            {record_name, some_rec},
            {attributes, record_info(fields, some_rec)},
            {ram_copies, Acc ++ [Node]}
        ]),
        ok = mnesia:sync_dirty(fun() ->
            lists:foreach(fun(Id) ->
                mnesia:write(Node, #some_rec{some_id = Id, some_int = Id}, write)
            end, lists:seq(1, ?NUM_RECORDS))
        end),
        Acc ++ [Node]
    end, [], Nodes),
    ok.

verify_data_in_table(Node) ->
    Data = mnesia:dirty_match_object(Node, #some_rec{_ = '_'}),
    ExpectedData = lists:map(fun(Id) -> #some_rec{some_id = Id, some_int = Id} end,
                             lists:seq(1, ?NUM_RECORDS)),
    case lists:sort(Data) of
        ExpectedData ->
            ok;
        _ ->
            {error, {unexpected_data, Data}}
    end.

remove_and_readd_node(Node, PeersAndNodes) ->
    ok = rpc:call(Node, mnesia, lkill, []),
    Tables = mnesia:system_info(tables),
    lists:foldl(fun(schema, Acc) ->
                        Acc;
                    (Table, Acc) ->
                        Nodes = mnesia:table_info(Table, ram_copies),
                        case lists:member(Node, Nodes) of
                            true ->
                                ok = mnesia:del_table_copy(Table, Node),
                                [fun() -> mnesia:add_table_copy(Table, Node, ram_copies) end | Acc];
                            false ->
                                Acc
                        end
    end, [], Tables),
    io:fwrite("Tables: ~p~n", [Tables]).
