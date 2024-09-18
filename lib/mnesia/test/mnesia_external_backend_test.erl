
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
    Data = [
        #some_rec{some_id = a, some_int = 1, some_string = "something" },
        #some_rec{some_id = b, some_int = 2, some_string = "anything"  },
        #some_rec{some_id = c, some_int = 3, some_string = "everything"},
        #some_rec{some_id = d, some_int = 4, some_string = "nothing"   }
    ],

    [Node] = ?acquire_nodes(1, Config),
    {atomic, ok} = mnesia:create_table(table, [
        {type, ordered_set},
        {record_name, some_rec},
        {attributes, record_info(fields, some_rec)},
        {ext_ets, [Node]}
    ]),

    ok = mnesia:activity(transaction, fun() ->
        lists:foreach(fun(Elem) -> mnesia:write(table, Elem, write) end, Data)
    end),

    ok = mnesia:backup("bk.BUP"),
    ok = mnesia:install_fallback("bk.BUP"),

    ok = mnesia:start().