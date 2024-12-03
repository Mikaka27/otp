-module(mnesia_external_backend_test).

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         suite/0, all/0, groups/0]).

-export([
    conversion_from_external_to_disc_copies_results_in_data_loss_after_node_restart/1,
    backup_and_restore_fails_with_external_backend/1
]).

-include("mnesia_test_lib.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(some_rec, {some_id :: atom(), some_int :: number(), some_string :: string()}).

-define(acquire(N, Config),
        mnesia_test_lib:prepare_test_case([{init_test_case, [mnesia]},
                                           delete_schema],
                                          N, Config, ?FILE, ?LINE)).

all() -> [
    backup_and_restore_fails_with_external_backend
].

groups() ->
    [].

init_per_testcase(Func, Conf) ->
    file:delete(ext_test_server:tab_to_filename(table)),
    file:delete("bup0.BUP"),
    file:delete("bup1.BUP"),
    file:delete("bup2.BUP"),
    {ok, _} = gen_server:start_link({local, ext_test_server}, ext_test_server, [self()],
                               [{timeout, infinity}
                                %%, {debug, [trace]}
                               ]),
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf),
    ok = gen_server:stop(ext_test_server).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,1}]}]}].

conversion_from_external_to_disc_copies_results_in_data_loss_after_node_restart(Config) when is_list(Config) ->
    Node = node(),
    Data = [
        #some_rec{some_id = a, some_int = 1, some_string = "something" },
        #some_rec{some_id = b, some_int = 2, some_string = "anything"  },
        #some_rec{some_id = c, some_int = 3, some_string = "everything"},
        #some_rec{some_id = d, some_int = 4, some_string = "nothing"   }
    ],

    [Node] = ?acquire(1, Config),
    ok = mnesia:create_schema([Node]),
    ok = mnesia:start(),
    {atomic, ok} = mnesia:add_backend_type(ext_ets, ext_test),
    {atomic, ok} = mnesia:add_backend_type(ext_dets, ext_test),
    {atomic, ok} = mnesia:create_table(table, [
        {type, ordered_set},
        {record_name, some_rec},
        {attributes, record_info(fields, some_rec)},
        {disc_copies, [Node]}
    ]),

    ok = mnesia:activity(transaction, fun() ->
        lists:foreach(fun(Elem) -> mnesia:write(table, Elem, write) end, Data)
    end),

    {atomic, ok} = mnesia:change_table_copy_type(table, Node, ext_ets),
    Data = mnesia:activity(transaction, fun() ->
        mnesia:match_object(table, #some_rec{_ = '_'}, read) end
    ),

    {atomic, ok} = mnesia:change_table_copy_type(table, Node, disc_copies),
    Data = mnesia:activity(transaction, fun() ->
        mnesia:match_object(table, #some_rec{_ = '_'}, read) end
    ),

    stopped = mnesia:stop(),
    ok = mnesia:start(),
    ok = mnesia:wait_for_tables([schema, table], 10000),

    Data = mnesia:activity(transaction, fun() ->
        mnesia:match_object(table, #some_rec{_ = '_'}, read) end
    ).

backup_and_restore_fails_with_external_backend(Config) when is_list(Config) ->
    Node = node(),
    Data1 = [
        #some_rec{some_id = a, some_int = 1, some_string = "1"},
        #some_rec{some_id = b, some_int = 2, some_string = "2"},
        #some_rec{some_id = c, some_int = 3, some_string = "3"}
    ],
    Data2 = [
        #some_rec{some_id = d, some_int = 4, some_string = "4"},
        #some_rec{some_id = e, some_int = 5, some_string = "5"},
        #some_rec{some_id = f, some_int = 6, some_string = "6"}
    ],

    [Node] = ?acquire_nodes(1, Config),
    ?match({atomic, ok}, mnesia:create_table(table, [
        {type, set},
        {record_name, some_rec},
        {attributes, record_info(fields, some_rec)},
        {ext_disc_only_copies, [Node]}
    ])),

    % ?match({atomic, ok}, mnesia:add_table_index(table, #some_rec.some_int)).
    ?match([], mnesia:dirty_match_object(table, #some_rec{_ = '_'})),
    % ?match(ok, mnesia:backup("bup0.BUP")),

    ?match(ok, mnesia:activity(transaction, fun() ->
        lists:foreach(fun(Elem) -> mnesia:write(table, Elem, write) end, Data1)
    end)),
    ?match(ok, mnesia:backup("bup1.BUP")),

    % ?match(ok, mnesia:activity(transaction, fun() ->
    %     lists:foreach(fun(Elem) -> mnesia:write(table, Elem, write) end, Data2)
    % end)),
    % ?match(ok, mnesia:backup("bup2.BUP")),

    % ?match(true, ets:whereis(table) =/= undefined),
    % ?match(ok, load_backup("bup0.BUP")).
    % ?match(true, ets:whereis(table) =/= undefined),
    % ?match([], mnesia:dirty_match_object(table, #some_rec{_ = '_'})).
    % [] = mnesia:dirty_index_read(table, 2, #some_rec.some_int),

    ?match(ok, load_backup("bup1.BUP")),
    Expected1 = sets:from_list(Data1),
    ?match(Expected1, sets:from_list(mnesia:dirty_match_object(table, #some_rec{_ = '_'}))).
    % [#some_rec{some_id = b, some_int = 2, some_string = "2"}] = mnesia:dirty_index_read(table, 2, #some_rec.some_int),

    % ?match(ok, load_backup("bup2.BUP")),
    % Expected2 = sets:from_list(lists:append(Data1, Data2)),
    % ?match(Expected2, sets:from_list(mnesia:dirty_match_object(table, #some_rec{_ = '_'}))).
    % [#some_rec{some_id = b, some_int = 2, some_string = "2"}] = mnesia:dirty_index_read(table, 2, #some_rec.some_int),
    % [#some_rec{some_id = e, some_int = 5, some_string = "5"}] = mnesia:dirty_index_read(table, 5, #some_rec.some_int).

load_backup(BUP) ->
    ok = mnesia:install_fallback(BUP),
    stopped = mnesia:stop(),
    timer:sleep(3000),
    ok = mnesia:start(),
    ok = mnesia:wait_for_tables([schema, table], 5000).
