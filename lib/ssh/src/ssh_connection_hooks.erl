%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
%%
-module(ssh_connection_hooks).
-moduledoc false.

-include("ssh.hrl").

-export([init_hooks/2,
         call_auth_completed/4,
         call_channel_open/4,
         call_channel_request/6,
         call_channel_data/5,
         call_channel_close/3,
         call_terminate/2,
         get_hook_state/2,
         get_all_hook_states/1]).

%% Initialize all connection hooks
init_hooks(ConnectionRef, Opts) ->
    case proplists:get_value(connection_hooks, Opts, []) of
        [] ->
            [];
        Hooks when is_list(Hooks) ->
            lists:filtermap(
                fun({Module, InitOpts}) ->
                    try Module:init(ConnectionRef, InitOpts) of
                        {ok, State} -> {true, {Module, State}};
                        {error, _} -> false
                    catch
                        _:_ -> false
                    end
                end, Hooks)
    end.

%% Call auth_completed on all hooks
call_auth_completed(User, Method, AuthData, Hooks) ->
    call_hooks_fold(
        fun(Module, State) ->
            try Module:auth_completed(User, Method, AuthData, State) of
                {ok, NewState} -> {continue, NewState};
                {disconnect, Reason} -> {stop, {disconnect, Reason}}
            catch
                error:undef -> {continue, State};
                _:_ -> {continue, State}
            end
        end, Hooks).

%% Call channel_open on all hooks
call_channel_open(ChannelType, ChannelId, Ssh, Hooks) ->
    call_hooks_fold(
        fun(Module, State) ->
            try Module:channel_open(ChannelType, ChannelId, State) of
                {allow, NewState} -> {continue, NewState};
                {deny, Reason, NewState} -> {stop, {deny, Reason, NewState}}
            catch
                error:undef -> {continue, State};
                _:_ -> {continue, State}
            end
        end, Hooks).

%% Call channel_request on all hooks
call_channel_request(ChannelId, RequestType, Data, WantReply, Ssh, Hooks) ->
    call_hooks_fold(
        fun(Module, State) ->
            try Module:channel_request(ChannelId, RequestType, Data, WantReply, State) of
                {allow, NewState} -> {continue, NewState};
                {deny, NewState} -> {stop, {deny, NewState}}
            catch
                error:undef -> {continue, State};
                _:_ -> {continue, State}
            end
        end, Hooks).

%% Call channel_data on all hooks
call_channel_data(ChannelId, DataType, Data, Ssh, Hooks) ->
    call_hooks_fold(
        fun(Module, State) ->
            try Module:channel_data(ChannelId, DataType, Data, State) of
                {ok, NewState} -> {continue, NewState};
                {throttle, NewState} -> {stop, {throttle, NewState}};
                {disconnect, Reason} -> {stop, {disconnect, Reason}}
            catch
                error:undef -> {continue, State};
                _:_ -> {continue, State}
            end
        end, Hooks).

%% Call channel_close on all hooks
call_channel_close(ChannelId, Ssh, Hooks) ->
    call_hooks_fold(
        fun(Module, State) ->
            try Module:channel_close(ChannelId, State) of
                {ok, NewState} -> {continue, NewState}
            catch
                error:undef -> {continue, State};
                _:_ -> {continue, State}
            end
        end, Hooks).

%% Call terminate on all hooks
call_terminate(Reason, Hooks) ->
    lists:foreach(
        fun({Module, State}) ->
            try Module:terminate(Reason, State)
            catch _:_ -> ok
            end
        end, Hooks).

%% Get state from a specific hook
get_hook_state(Module, Hooks) ->
    case lists:keyfind(Module, 1, Hooks) of
        {Module, State} -> {ok, State};
        false -> {error, not_found}
    end.

%% Get all hook states
get_all_hook_states(Hooks) ->
    [{Module, State} || {Module, State} <- Hooks].

%% Internal helper to fold over hooks
call_hooks_fold(Fun, Hooks) ->
    call_hooks_fold(Fun, Hooks, []).

call_hooks_fold(_Fun, [], Acc) ->
    {ok, lists:reverse(Acc)};
call_hooks_fold(Fun, [{Module, State} | Rest], Acc) ->
    case Fun(Module, State) of
        {continue, NewState} ->
            call_hooks_fold(Fun, Rest, [{Module, NewState} | Acc]);
        {stop, Result} ->
            %% Return updated hooks up to this point plus remaining unchanged
            UpdatedHooks = lists:reverse(Acc) ++ [{Module, element(tuple_size(Result), Result)} | Rest],
            {stop, Result, UpdatedHooks}
    end.
