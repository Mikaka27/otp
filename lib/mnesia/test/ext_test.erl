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

-module(ext_test).

-include("ext_test_server.hrl").

%% Initializations
-export([init_backend/0, add_aliases/1, remove_aliases/1,
	 check_definition/4, semantics/2]).

-export([
	 create_table/3, load_table/4,
	 delete_table/2, close_table/2, sync_close_table/2,

	 sender_init/4,
	 receiver_first_message/4, receive_data/5, receive_done/4,

	 index_is_consistent/3, is_index_consistent/2,

	 real_suffixes/0, tmp_suffixes/0,

	 info/3,
	 fixtable/3,
	 validate_key/6, validate_record/6,

	 first/2, last/2, next/3, prev/3, slot/3,

	 insert/3, update_counter/4,
	 lookup/3,
	 delete/3, match_delete/3,
	 select/1, select/3, select/4, repair_continuation/2
	]).

semantics(ext_ram_copies, storage) -> ram_copies;
semantics(ext_ram_copies, types  ) -> [set, ordered_set, bag];
semantics(ext_ram_copies, index_types) -> [ordered];
semantics(ext_disc_only_copies, storage) -> disc_only_copies;
semantics(ext_disc_only_copies, types  ) -> [set, bag];
semantics(ext_disc_only_copies, index_types) -> [bag];
semantics(_Alias, _) ->
    undefined.

init_backend() ->
    ?DBG(),
    %% cheat and stuff a marker in mnesia_gvar
    K = backend_init_marker(),
    case try ets:lookup_element(mnesia_gvar, K, 2) catch _:_ -> error end of
        error ->
            mnesia_lib:set(K, true);
        Other ->
            error({backend_already_initialized, {?MODULE, Other}})
    end,
    ok.

add_aliases(_As) ->
    ?DBG(_As),
    true = mnesia_lib:val(backend_init_marker()),
    ok.

remove_aliases(_) ->
    ok.


%% Table operations

check_definition(ext_ram_copies, _Tab, _Nodes, _Props) ->
    ?DBG("~p ~p ~p~n", [ext_test_server:tab_to_list(_Tab), _Nodes, _Props]),
    ok;
check_definition(ext_disc_only_copies, _Tab, _Nodes, _Props) ->
    ?DBG("~p ~p ~p~n", [ext_test_server:tab_to_list(_Tab), _Nodes, _Props]),
    ok.

create_table(Alias, Tab, Props) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, Props}).

delete_table(Alias, Tab) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab}).

load_table(Alias, Tab, LoadReason, Cs) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, LoadReason, Cs}).

sender_init(Alias, Tab, RemoteStorage, Pid) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, RemoteStorage, Pid}).

receiver_first_message(Sender, {first, Size}, _Alias, Tab) ->
    ?DBG({first, Size}),
    {Size, {Tab, Sender}}.

receive_data(Data, Alias, Name, Sender, State) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Data, Alias, Name, Sender, State}).

receive_done(_Alias, _Tab, _Sender, _State) ->
    ?DBG({done, _State}),
    ok.

close_table(Alias, Tab) -> sync_close_table(Alias, Tab).

sync_close_table(Alias, Tab) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab}).

fixtable(Alias, Tab, Bool) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, Bool}).

info(Alias, Tab, Type) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, Type}).

real_suffixes() ->
    [".dat"].

tmp_suffixes() ->
    [].

%% Index

index_is_consistent(_Alias, _Ix, _Bool) -> ok.  % Ignore for now
is_index_consistent(_Alias, _Ix) -> false.      % Always rebuild

%% Record operations

validate_record(_Alias, _Tab, RecName, Arity, Type, _Obj) ->
    {RecName, Arity, Type}.

validate_key(_Alias, _Tab, RecName, Arity, Type, _Key) ->
    {RecName, Arity, Type}.

insert(Alias, Tab, Obj) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, Obj}).

lookup(Alias, Tab, Obj) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, Obj}).

delete(Alias, Tab, Key) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, Key}).

match_delete(Alias, Tab, Pat) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, Pat}).

first(Alias, Tab) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab}).

last(Alias, Tab) -> first(Alias, Tab).

next(Alias, Tab, Key) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, Key}).

prev(Alias, Tab, Key) ->
    next(Alias, Tab, Key).

slot(Alias, Tab, Pos) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, Pos}).

update_counter(Alias, Tab, C, Val) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, C, Val}).

select(Continuation) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Continuation}).

select(Alias, Tab, Ms) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, Ms}).

select(Alias, Tab, Ms, Limit) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Alias, Tab, Ms, Limit}).

repair_continuation(Cont, Ms) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Cont, Ms}).

backend_init_marker() ->
    {test, ext_test, backend_init}.