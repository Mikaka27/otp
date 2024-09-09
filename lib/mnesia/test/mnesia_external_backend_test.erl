-module(mnesia_external_backend_test).

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         suite/0, all/0, groups/0]).

-export([one_node_dies_and_restarts_when_there_is_already_new_node_in_cluster/1]).

-include("mnesia_test_lib.hrl").

-define(init(N, Config),
	mnesia_test_lib:prepare_test_case([{init_test_case, [mnesia]},
					   delete_schema,
					   {reload_appls, [mnesia]}],
					  N, Config, ?FILE, ?LINE)).
-define(acquire(N, Config),
	mnesia_test_lib:prepare_test_case([{init_test_case, [mnesia]},
					   delete_schema,
					   {reload_appls, [mnesia]},
					   create_schema,
					   {start_appls, [mnesia]}],
					  N, Config, ?FILE, ?LINE)).
-define(acquire_schema(N, Config),
	mnesia_test_lib:prepare_test_case([{init_test_case, [mnesia]},
					    delete_schema,
					    {reload_appls, [mnesia]},
					    create_schema],
					  N, Config, ?FILE, ?LINE)).
-define(cleanup(N, Config),
	mnesia_test_lib:prepare_test_case([{reload_appls, [mnesia]}],
					  N, Config, ?FILE, ?LINE)).
-define(trans(Fun),
	?match({atomic, ok}, mnesia:transaction(Fun))).

all() -> 
    [one_node_dies_and_restarts_when_there_is_already_new_node_in_cluster].

groups() ->
    [].

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,3}]}]}].

append_unique([], List) -> List;
append_unique([H|R], List) ->
    case lists:member(H, List) of
	true -> append_unique(R, List);
	false -> [H | append_unique(R, List)]
    end.

pick_nodes(all, Nodes) ->
    pick_nodes(length(Nodes), Nodes);
pick_nodes(N, [H | T]) when N > 0 ->
    [H | pick_nodes(N - 1, T)];
pick_nodes(0, _Nodes) ->
    [];
pick_nodes(_N, []) ->
	throw(raise).

one_node_dies_and_restarts_when_there_is_already_new_node_in_cluster(Config) when is_list(Config) ->
	NodeList1 = mnesia_test_lib:lookup_config(nodes, Config),
    NodeList2 = mnesia_test_lib:lookup_config(nodenames, Config), %% For testserver
    NodeList3 = append_unique(NodeList1, NodeList2),
    This = node(),
    All = [This | lists:delete(This, NodeList3)],
	[N1, N2, N3] = mnesia_test_lib:init_nodes(pick_nodes(3, All), ?FILE, ?LINE),
	Ext = proplists:get_value(default_properties, Config, ?BACKEND),

	{[ok, ok, ok], []} = rpc:multicall([N1, N2, N3], mnesia, delete_schema, [[N1, N2, N3]]),

    ok = rpc:call(N1, mnesia, create_schema, [[N1, N2], Ext]),
	{[ok, ok], []} = rpc:multicall([N1, N2], mnesia, start, []),
	{atomic, ok} = rpc:call(N1, mnesia, create_table, [table, [{ext_ets, [N1, N2]}]]),

    stopped = rpc:call(N2, mnesia, stop, []),
	
    {atomic, ok} = rpc:call(N1, mnesia, add_table_copy, [schema, N3, ram_copies]),
	ok = rpc:call(N3, mnesia, start, []),
    {atomic, ok} = rpc:call(N3, mnesia, add_backend_type, [ext_ets, ext_test]),
    {atomic, ok} = rpc:call(N3, mnesia, add_backend_type, [ext_dets, ext_test]),
    
    % {atomic, ok} = rpc:call(N1, mnesia, add_table_copy, [table, N3, ext_ets]),
    {atomic, ok} = rpc:call(N3, mnesia, create_table, [table, [{ext_ets, [N3]}]]),

    [N1] = rpc:call(N3, mnesia, change_config, [extra_db_nodes, [N1]]),
	ok = rpc:call(N2, mnesia, start, []).
