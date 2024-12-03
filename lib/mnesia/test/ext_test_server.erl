%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2022. All Rights Reserved.
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

-module(ext_test_server).

-include("ext_test_server.hrl").

%% This process is supposed to emulate external database process, it should not be linked
%% to mnesia_monitor

-export([tab_to_filename/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, terminate/2, code_change/3]).

init(_) ->
    ?DBG(),
    {ok, []}.

create_table(ext_ram_copies, Tab, Props) when is_atom(Tab) ->
    case catch mnesia_lib:val({?MODULE, Tab}) of
        Tid when is_reference(Tid) ->
            ?DBG("create_table, Alias: ext_ram_copies, Tab: ~p(~p) is already created~n", [tab_to_list(Tab), Tid]);
        _ ->
            ?DBG("create_table, Alias: ext_ram_copies, Tab: ~p~n", [tab_to_list(Tab)]),
            Tid = ets:new(Tab, [public, proplists:get_value(type, Props, set), {keypos, 2}]),
            mnesia_lib:set({?MODULE, Tab}, Tid),
            ?DBG("create_table, Alias, ext_ram_copies, Tab: ~p(~p)~n", [tab_to_list(Tab), Tid])
    end,
    ok;
create_table(ext_disc_only_copies, Tab, Props) when is_atom(Tab) ->
    case catch mnesia_lib:val({?MODULE, Tab}) of
        Tab ->
            ?DBG("create_table, Alias: ext_disc_only_copies, Tab: ~p(~p) is already created~n", [tab_to_list(Tab), Tab]);
        _ ->
            ?DBG("create_table, Alias: ext_disc_only_copies, Tab: ~p~n", [tab_to_list(Tab)]),
            File = tab_to_filename(Tab),
            false = filelib:is_regular(File),
            {ok, Tab} = dets:open_file(Tab, [{type, proplists:get_value(type, Props, set)}, {keypos, 2}, {file, File}]),
            mnesia_lib:set({?MODULE, Tab}, Tab),
            ?DBG("create_table Alias: ext_disc_only_copies after dets:open_file, Tab: ~p~n", [tab_to_list(Tab)])
    end,
    ok;
create_table(ext_ram_copies, Tag={Tab, index, {_Where, Type}}, _Opts) ->
    case catch mnesia_lib:val({?MODULE, Tag}) of
        Tid when is_reference(Tid) ->
            ?DBG("create_table, Alias: ext_ram_copies, Tab: ~p(~p) is already created~n", [tab_to_list(Tag), Tid]);
        _ ->
            ?DBG("create_table, Alias: ext_ram_copies, Tab: ~p~n", [tab_to_list(Tag)]),
            Tid = ets:new(tab_to_atom(Tag), [public, type_to_type_in_alias(ext_ram_copies, Type)]),
            mnesia_lib:set({?MODULE, Tag}, Tid),
            ?DBG("create_table, Alias, ext_ram_copies, Tab: ~p(~p)~n", [tab_to_list(Tag), Tid])
    end,
    ok;
create_table(ext_disc_only_copies, Tag={Tab, index, {_Where, Type}}, _Opts) ->
    case catch mnesia_lib:val({?MODULE, Tag}) of
        Tag ->
            ?DBG("create_table, Alias: ext_disc_only_copies, Tab: ~p(~p) is already created~n", [tab_to_list(Tag), Tag]);
        _ ->
            ?DBG("create_table, Alias: ext_disc_only_copies, Tab: ~p~n", [tab_to_list(Tag)]),
            File = tab_to_filename(Tag),
            false = filelib:is_regular(File),
            {ok, Tag} = dets:open_file(Tag, [{type, type_to_type_in_alias(ext_disc_only_copies, Type)}, {file, File}]),
            mnesia_lib:set({?MODULE, Tag}, Tag),
            ?DBG("create_table Alias: ext_disc_only_copies after dets:open_file, Tab: ~p~n", [tab_to_list(Tag)])
    end,
    ok;
create_table(ext_ram_copies, Tag={_Tab, retainer, {ChkPNumber, Node}}, _Opts) ->
    case catch mnesia_lib:val({?MODULE, Tag}) of
        Tid when is_reference(Tid) ->
            ?DBG("create_table, Alias: ext_ram_copies, Tab: ~p(~p) is already created~n", [tab_to_list(Tag), Tid]);
        _ ->
            ?DBG("create_table, Alias: ext_ram_copies, Tab: ~p~n", [tab_to_list(Tag)]),
            Tid = ets:new(tab_to_atom(Tag), [set, public, {keypos, 2}]),
            mnesia_lib:set({?MODULE, Tag}, Tid),
            ?DBG("create_table, Alias, ext_ram_copies, Tab: ~p(~p)~n", [tab_to_list(Tag), Tid])
    end,
    ok;
create_table(ext_disc_only_copies, Tag={_Tab, retainer, {ChkPNumber, Node}}, _Opts) ->
    case catch mnesia_lib:val({?MODULE, Tag}) of
        Tag ->
            ?DBG("create_table, Alias: ext_disc_only_copies, Tab: ~p(~p) is already created~n", [tab_to_list(Tag), Tag]);
        _ ->
            ?DBG("create_table, Alias: ext_disc_only_copies, Tab: ~p~n", [tab_to_list(Tag)]),
            File = tab_to_filename(Tag),
            false = filelib:is_regular(File),
            {ok, Tag} = dets:open_file(Tag, [{type, set}, {keypos, 2}, {file, File}]),
            mnesia_lib:set({?MODULE, Tag}, Tag),
            ?DBG("create_table, Alias: ext_disc_only_copies after dets:open_file, Tab: ~p~n", [tab_to_list(Tag)])
    end,
    ok.

receive_data(Data, ext_ram_copies, Name, Sender, {Name, Tab, Sender} = State) ->
    ?DBG({Data, ext_ram_copies, Name, Sender, {Name, tab_to_list(Tab), Sender}}),
    true = ets:insert(Tab, Data),
    {more, State};
receive_data(Data, ext_disc_only_copies, Name, Sender, {Name, Tab, Sender} = State) ->
    ?DBG({Data, ext_disc_only_copies, Name, Sender, {Name, tab_to_list(Tab), Sender}}),
    ok = dets:insert(Tab, Data),
    {more, State};
receive_data(Data, Alias, Tab, Sender, {Name, Sender} = State) ->
    ?DBG({Data, Alias, tab_to_list(Tab), State}),
    receive_data(Data, Alias, Tab, Sender, {Name, mnesia_lib:val({?MODULE, Tab}), Sender}).

select(Alias, Tab, Ms) ->
    Res = select(Alias, Tab, Ms, 100000),
    select_1(Alias, Res).

select_1(_Alias, '$end_of_table') -> [];
select_1(ext_ram_copies, {Acc, C}) ->
    case ets:select(C) of
	'$end_of_table' -> Acc;
	{New, Cont} ->
	    select_1(ext_ram_copies, {New ++ Acc, Cont})
    end;
select_1(ext_disc_only_copies, {Acc, C}) ->
    case dets:select(C) of
    '$end_of_table' -> Acc;
    {New, Cont} ->
        select_1(ext_disc_only_copies, {New ++ Acc, Cont})
    end.

select(ext_ram_copies, Tab, Ms, Limit) when is_integer(Limit); Limit =:= infinity ->
    ?DBG({ext_ram_copies, tab_to_list(Tab), Ms, Limit}),
    ets:select(mnesia_lib:val({?MODULE, Tab}), Ms, Limit);
select(ext_disc_only_copies, Tab, Ms, Limit) when is_integer(Limit); Limit =:= infinity ->
    ?DBG({ext_disc_only_copies, tab_to_list(Tab), Ms, Limit}),
    dets:select(mnesia_lib:val({?MODULE, Tab}), Ms, Limit).

handle_call({create_table, Alias, Tab, Props}, _From, State) ->
    ?DBG({create_table, Alias, tab_to_list(Tab), Props}),
    Res = create_table(Alias, Tab, Props),
    {reply, Res, State};

handle_call({delete_table, ext_ram_copies, Tab}, _From, State) ->
    ?DBG({delete_table, ext_ram_copies, tab_to_list(Tab)}),
    try
        ets:delete(mnesia_lib:val({?MODULE, Tab}))
    catch _:_ ->
        ?DBG("delete_table, ~p~n", {double_delete, tab_to_list(Tab)})
    after
        mnesia_lib:unset({?MODULE, Tab})
    end,
    {reply, ok, State};
handle_call({delete_table, ext_disc_only_copies, Tab}, _From, State) ->
    ?DBG({delete_table, ext_disc_only_copies, tab_to_list(Tab)}),
    try
        file:delete(tab_to_filename(Tab))
    catch _:_ ->
        ?DBG("delete_table, ~p~n", {double_delete, tab_to_list(Tab)})
    after
        mnesia_lib:unset({?MODULE, Tab})
    end,
    {reply, ok, State};

handle_call({load_table, _Alias, _Tab, init_index, _Cs}, _From, State) ->
    ?DBG({load_table, _Alias, tab_to_list(_Tab), init_index, _Cs}),
    {reply, ok, State};
handle_call({load_table, Alias, Tab, restore, Cs}, _From, State) ->
    ?DBG({load_table, Alias, tab_to_list(Tab), restore, Cs}),
    Res = create_table(Alias, Tab, mnesia_schema:cs2list(Cs)),
    {reply, Res, State};
handle_call({load_table, _Alias, Tab, _LoadReason, Cs}, _From, State) ->
    ?DBG({load_table, _Alias, tab_to_list(Tab), _LoadReason, Cs}),
    {reply, ok, State};

handle_call({sender_init, Alias, Tab, _RemoteStorage, _Pid}, _From, State) ->
    ?DBG({sender_init, Alias, tab_to_list(Tab), _RemoteStorage, _Pid}),
    KeysPerTransfer = 100,
    Res = {standard,
        fun() -> mnesia_lib:db_init_chunk({ext, Alias, ?MODULE}, Tab, KeysPerTransfer) end,
        fun(Cont) -> mnesia_lib:db_chunk({ext, Alias, ?MODULE}, Cont) end},
    {reply, Res, State};

handle_call({receive_data, Data, Alias, Name, Sender, MnesiaState}, _From, State) ->
    ?DBG({receive_data, Data, Alias, Name, Sender, MnesiaState}),
    Res = receive_data(Data, Alias, Name, Sender, MnesiaState),
    {reply, Res, State};

handle_call({sync_close_table, ext_ram_copies, _Tab}, _From, State) ->
    ?DBG({sync_close_table, ext_ram_copies, tab_to_list(_Tab)}),
    {reply, ok, State};
handle_call({sync_close_table, ext_disc_only_copies, Tab}, _From, State) ->
    ?DBG({sync_close_table, ext_disc_only_copies, tab_to_list(Tab)}),
    ok = dets:sync(Tab),
    ok = dets:close(Tab),
    {reply, ok, State};

handle_call({fixtable, ext_ram_copies, Tab, Bool}, _From, State) ->
    ?DBG({fixtable, ext_ram_copies, tab_to_list(Tab), Bool}),
    Res = ets:safe_fixtable(mnesia_lib:val({?MODULE, Tab}), Bool),
    {reply, Res, State};
handle_call({fixtable, ext_disc_only_copies, Tab, Bool}, _From, State) ->
    ?DBG({fixtable, ext_disc_only_copies, tab_to_list(Tab), Bool}),
    Res = dets:safe_fixtable(mnesia_lib:val({?MODULE, Tab}), Bool),
    {reply, Res, State};

handle_call({info, ext_ram_copies, Tab, Type}, _From, State) ->
    ?DBG({info, ext_ram_copies, tab_to_list(Tab), Type}),
    Tid = mnesia_lib:val({?MODULE, Tab}),
    Res = try ets:info(Tid, Type) of
	Val -> Val
    catch _:_ ->
	    undefined
    end,
    {reply, Res, State};
handle_call({info, ext_disc_only_copies, Tab, Type}, _From, State) ->
    ?DBG({info, ext_disc_only_copies, tab_to_list(Tab), Type}),
    Tid = mnesia_lib:val({?MODULE, Tab}),
    Res = try dets:info(Tid, Type) of
    Val -> Val
    catch _:_ ->
        undefined
    end,
    {reply, Res, State};

handle_call({insert, ext_ram_copies, Tab, Obj}, _From, State) ->
    ?DBG({insert, ext_ram_copies, tab_to_list(Tab), Obj}),
    true = ets:insert(mnesia_lib:val({?MODULE, Tab}), Obj),
    {reply, ok, State};
handle_call({insert, ext_disc_only_copies, Tab, Obj}, _From, State) ->
    ?DBG({insert, ext_disc_only_copies, tab_to_list(Tab), Obj}),
    ok = dets:insert(mnesia_lib:val({?MODULE, Tab}), Obj),
    {reply, ok, State};

handle_call({lookup, ext_ram_copies, Tab, Key}, _From, State) ->
    ?DBG({lookup, ext_ram_copies, tab_to_list(Tab), Key}),
    Res = ets:lookup(mnesia_lib:val({?MODULE, Tab}), Key),
    {reply, Res, State};
handle_call({lookup, ext_disc_only_copies, Tab, Key}, _From, State) ->
    ?DBG({lookup, ext_disc_only_copies, tab_to_list(Tab), Key}),
    Res = dets:lookup(mnesia_lib:val({?MODULE, Tab}), Key),
    {reply, Res, State};

handle_call({delete, ext_ram_copies, Tab, Key}, _From, State) ->
    ?DBG({delete, ext_ram_copies, tab_to_list(Tab), Key}),
    Res = ets:delete(mnesia_lib:val({?MODULE, Tab}), Key),
    {reply, Res, State};
handle_call({delete, ext_disc_only_copies, Tab, Key}, _From, State) ->
    ?DBG({delete, ext_disc_only_copies, tab_to_list(Tab), Key}),
    Res = dets:delete(mnesia_lib:val({?MODULE, Tab}), Key),
    {reply, Res, State};

handle_call({match_delete, ext_ram_copies, Tab, Pat}, _From, State) ->
    ?DBG({match_delete, ext_ram_copies, tab_to_list(Tab), Pat}),
    Res = ets:match_delete(mnesia_lib:val({?MODULE, Tab}), Pat),
    {reply, Res, State};
handle_call({match_delete, ext_disc_only_copies, Tab, Pat}, _From, State) ->
    ?DBG({match_delete, ext_disc_only_copies, tab_to_list(Tab), Pat}),
    Res = dets:match_delete(mnesia_lib:val({?MODULE, Tab}), Pat),
    {reply, Res, State};

handle_call({first, ext_ram_copies, Tab}, _From, State) ->
    ?DBG({first, ext_ram_copies, tab_to_list(Tab)}),
    Res = ets:first(mnesia_lib:val({?MODULE, Tab})),
    {reply, Res, State};
handle_call({first, ext_disc_only_copies, Tab}, _From, State) ->
    ?DBG({first, ext_disc_only_copies, tab_to_list(Tab)}),
    Res = dets:first(mnesia_lib:val({?MODULE, Tab})),
    {reply, Res, State};

handle_call({next, ext_ram_copies, Tab, Key}, _From, State) ->
    ?DBG({next, ext_ram_copies, tab_to_list(Tab), Key}),
    Res = ets:next(mnesia_lib:val({?MODULE, Tab}), Key),
    {reply, Res, State};
handle_call({next, ext_disc_only_copies, Tab, Key}, _From, State) ->
    ?DBG({next, ext_disc_only_copies, tab_to_list(Tab), Key}),
    Res = dets:next(mnesia_lib:val({?MODULE, Tab}), Key),
    {reply, Res, State};

handle_call({slot, ext_ram_copies, Tab, Pos}, _From, State) ->
    ?DBG({slot, ext_ram_copies, tab_to_list(Tab), Pos}),
    Res = ets:slot(mnesia_lib:val({?MODULE, Tab}), Pos),
    {reply, Res, State};
handle_call({slot, ext_disc_only_copies, Tab, Pos}, _From, State) ->
    ?DBG({slot, ext_disc_only_copies, tab_to_list(Tab), Pos}),
    Res = dets:slot(mnesia_lib:val({?MODULE, Tab}), Pos),
    {reply, Res, State};

handle_call({update_counter, ext_ram_copies, Tab, C, Val}, _From, State) ->
    ?DBG({update_counter, ext_ram_copies, tab_to_list(Tab), C, Val}),
    Res = ets:update_counter(mnesia_lib:val({?MODULE, Tab}), C, Val),
    {reply, Res, State};
handle_call({update_counter, ext_disc_only_copies, Tab, C, Val}, _From, State) ->
    ?DBG({update_counter, ext_disc_only_copies, tab_to_list(Tab), C, Val}),
    Res = dets:update_counter(mnesia_lib:val({?MODULE, Tab}), C, Val),
    {reply, Res, State};

handle_call({select, '$end_of_table' = End}, _From, State) ->
    ?DBG({select, End}),
    {reply, End, State};
handle_call({select, {ext_ram_copies, C}}, _From, State) ->
    ?DBG({select, {ext_ram_copies, C}}),
    Res = ets:select(C),
    {reply, Res, State};
handle_call({select, {ext_disc_only_copies, C}}, _From, State) ->
    ?DBG({select, {ext_disc_only_copies, C}}),
    Res = dets:select(C),
    {reply, Res, State};

handle_call({select, Alias, Tab, Ms}, _From, State) ->
    ?DBG({select, Alias, tab_to_list(Tab), Ms}),
    Res = select(Alias, Tab, Ms),
    {reply, Res, State};

handle_call({select, Alias, Tab, Ms, Limit}, _From, State) ->
    ?DBG({select, Alias, tab_to_list(Tab), Ms, Limit}),
    Res = select(Alias, Tab, Ms, Limit),
    {reply, Res, State};

handle_call({repair_continuation, Cont, Ms}, _From, State) ->
    ?DBG({repair_continuation, Cont, Ms}),
    Res = case element(1, Cont) of
        dets_cont ->
            dets:repair_continuation(Cont, Ms);
        _ ->
            ets:repair_continuation(Cont, Ms)
    end,
    {reply, Res, State}.

terminate(Reason, _State) ->
    ?DBG(Reason).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

tab_to_atom(Tab) ->
    list_to_atom(tab_to_list(Tab)).
tab_to_list(Tab) when is_atom(Tab) ->
    atom_to_list(Tab);
tab_to_list({Tab, index, {Where, Type}}) ->
    atom_to_list(Tab) ++ "_index_" ++ integer_to_list(Where) ++ "_" ++ atom_to_list(Type);
tab_to_list({Tab, retainer, {ChkPNumber, Node}}) ->
    atom_to_list(Tab) ++ "_retainer_" ++ integer_to_list(ChkPNumber) ++ "_" ++ atom_to_list(Node).

type_to_type_in_alias(ext_ram_copies, ordered) ->
    ordered_set;
type_to_type_in_alias(_, Type) ->
    Type.

tab_to_filename(Tab) ->
    FName = tab_to_list(Tab) ++ ".dat.ext",
    mnesia_lib:dir(FName).