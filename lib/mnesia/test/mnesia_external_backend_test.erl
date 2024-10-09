
-module(mnesia_external_backend_test).

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         suite/0, all/0, groups/0]).

-export([backup_and_restore_fails_with_external_backend/1]).

-include("mnesia_test_lib.hrl").

-record(some_rec, {some_id :: atom(), some_int :: number(), some_string :: string()}).

all() -> 
    [backup_and_restore_fails_with_external_backend].

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

suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,1}]}]}].

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
    {atomic, ok} = mnesia:create_table(table, [
        {type, ordered_set},
        {record_name, some_rec},
        {attributes, record_info(fields, some_rec)},
        {ext_dets, [Node]}
    ]),
    {atomic, ok} = mnesia:add_table_index(table, #some_rec.some_int),
    ok = mnesia:backup("bup0.BUP"),

    ok = mnesia:activity(transaction, fun() ->
        lists:foreach(fun(Elem) -> mnesia:write(table, Elem, write) end, Data1)
    end),
    ok = mnesia:backup("bup1.BUP"),

    ok = mnesia:activity(transaction, fun() ->
        lists:foreach(fun(Elem) -> mnesia:write(table, Elem, write) end, Data2)
    end),
    ok = mnesia:backup("bup2.BUP"),

    ok = load_backup("bup0.BUP"),
    [] = mnesia:dirty_match_object(table, #some_rec{_ = '_'}),
    [] = mnesia:dirty_index_read(table, 2, #some_rec.some_int),

    ok = load_backup("bup1.BUP"),
    Data1 = mnesia:dirty_match_object(table, #some_rec{_ = '_'}),
    [#some_rec{some_id = b, some_int = 2, some_string = "2"}] = mnesia:dirty_index_read(table, 2, #some_rec.some_int),

    ok = load_backup("bup2.BUP"),
    Data = lists:append(Data1, Data2),
    Data = mnesia:dirty_match_object(table, #some_rec{_ = '_'}),
    [#some_rec{some_id = b, some_int = 2, some_string = "2"}] = mnesia:dirty_index_read(table, 2, #some_rec.some_int),
    [#some_rec{some_id = e, some_int = 5, some_string = "5"}] = mnesia:dirty_index_read(table, 5, #some_rec.some_int).

load_backup(BUP) ->
    ok = mnesia:install_fallback(BUP),
    stopped = mnesia:stop(),
    timer:sleep(3000),
    ok = mnesia:start(),
    ok = mnesia:wait_for_tables([table], 5000).