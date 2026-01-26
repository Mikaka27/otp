%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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

-module(ssh_connection_hook).
-moduledoc """
Behavior for implementing custom SSH connection hooks.

This behavior allows applications to hook into SSH connection lifecycle events
and maintain mutable state throughout the connection lifetime.

## Use Cases

- Dynamic authorization and access control
- Connection-level rate limiting and quotas
- Audit logging with correlation IDs
- Multi-factor authentication flows
- Custom protocol negotiation
- Resource accounting per connection

## Example

```erlang
-module(my_connection_hook).
-behaviour(ssh_connection_hook).

-export([init/2, auth_completed/4, channel_open/4, channel_request/5,
         channel_data/5, channel_close/3, terminate/2]).

init(_ConnectionRef, _Opts) ->
    {ok, #{bytes_transferred => 0, channels => []}}.

auth_completed(User, _Method, _AuthData, State) ->
    {ok, State#{user => User}}.

channel_open(_Type, ChannelId, State) ->
    Channels = maps:get(channels, State, []),
    {allow, State#{channels => [ChannelId | Channels]}}.

channel_request(ChannelId, "exec", Cmd, _WantReply, State) ->
    %% Check if user is allowed to execute this command
    case is_allowed(maps:get(user, State), Cmd) of
        true -> {allow, State};
        false -> {deny, State}
    end;
channel_request(_ChId, _Type, _Data, _WantReply, State) ->
    {allow, State}.

channel_data(_ChannelId, _DataType, Data, State) ->
    Bytes = maps:get(bytes_transferred, State, 0) + byte_size(Data),
    if Bytes > 1000000 ->
        {disconnect, "Quota exceeded"};
    true ->
        {ok, State#{bytes_transferred => Bytes}}
    end.

channel_close(ChannelId, State) ->
    Channels = lists:delete(ChannelId, maps:get(channels, State, [])),
    {ok, State#{channels => Channels}}.

terminate(_Reason, _State) ->
    ok.
```

## Configuration

```erlang
%% Single hook
ssh:daemon(Port, [{connection_hooks, [{my_hook, []}]}]).

%% Multiple hooks (called in order)
ssh:daemon(Port, [{connection_hooks, [
    {audit_hook, []},
    {auth_hook, []},
    {quota_hook, []}
]}]).
```
""".
-moduledoc(#{since => "OTP 28.0"}).

-doc """
Initialize the session handler state.

Called when the SSH connection is established, before authentication.
""".
-doc(#{since => "OTP 28.0"}).
-callback init(ConnectionRef :: ssh:connection_ref(), 
               Opts :: proplists:proplist()) ->
    {ok, State :: term()} | 
    {error, Reason :: term()}.

-doc """
Called after successful authentication.

The handler can inspect authentication details and update session state.
Returning `{disconnect, Reason}` will terminate the connection.
""".
-doc(#{since => "OTP 28.0"}).
-callback auth_completed(User :: string(),
                         Method :: string(),
                         AuthData :: term(),
                         State :: term()) ->
    {ok, NewState :: term()} | 
    {disconnect, Reason :: term()}.

-doc """
Called when a client requests to open a new channel.

The handler can allow or deny the channel open request.
""".
-doc(#{since => "OTP 28.0"}).
-callback channel_open(ChannelType :: string(),
                       ChannelId :: ssh:channel_id(),
                       State :: term()) ->
    {allow, NewState :: term()} | 
    {deny, Reason :: string(), NewState :: term()}.

-doc """
Called when a channel request is received (exec, shell, subsystem, etc).

The handler can allow or deny the request.
""".
-doc(#{since => "OTP 28.0"}).
-callback channel_request(ChannelId :: ssh:channel_id(),
                          RequestType :: string(),
                          Data :: term(),
                          WantReply :: boolean(),
                          State :: term()) ->
    {allow, NewState :: term()} | 
    {deny, NewState :: term()}.

-doc """
Called when data is received on a channel.

The handler can inspect data for rate limiting, logging, etc.
Returning `{throttle, NewState}` will pause reading from the channel.
Returning `{disconnect, Reason}` will terminate the connection.
""".
-doc(#{since => "OTP 28.0"}).
-callback channel_data(ChannelId :: ssh:channel_id(),
                       DataType :: non_neg_integer(),
                       Data :: binary(),
                       State :: term()) ->
    {ok, NewState :: term()} | 
    {throttle, NewState :: term()} | 
    {disconnect, Reason :: term()}.

-doc """
Called when a channel is closed.

The handler can clean up channel-specific state.
""".
-doc(#{since => "OTP 28.0"}).
-callback channel_close(ChannelId :: ssh:channel_id(),
                        State :: term()) ->
    {ok, NewState :: term()}.

-doc """
Called when the SSH connection is terminated.

The handler should clean up any resources.
""".
-doc(#{since => "OTP 28.0"}).
-callback terminate(Reason :: term(), State :: term()) ->
    ok.

-optional_callbacks([auth_completed/4, channel_open/3, channel_request/5,
                     channel_data/5, channel_close/3, terminate/2]).
