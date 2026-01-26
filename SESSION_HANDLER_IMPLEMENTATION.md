# Session Handler Implementation Summary

## Overview
Replace the `auth_context` approach with a comprehensive session handler system that allows applications to hook into SSH connection lifecycle events.

## Key Changes

### 1. ssh.hrl - Record Definition
```erlang
-record(ssh, {
    % ... existing fields ...
    session_handler = undefined,  % {Module, State} | undefined
    % Remove: auth_context = #{}
}).
```

### 2. New Module: ssh_session_handler.erl
Behavior module defining callbacks:
- `init/2` - Initialize session state
- `auth_completed/4` - Called after successful authentication
- `channel_open/3` - Hook into channel open requests
- `channel_request/5` - Hook into channel requests (exec, shell, etc)
- `channel_data/5` - Hook into data transfer
- `channel_close/3` - Hook into channel close
- `terminate/2` - Cleanup on connection close

### 3. ssh_connection_handler.erl - Integration Points

#### Initialization
```erlang
init_ssh_record(Role, Socket, PeerAddr, Opts) ->
    SessionHandler = case ?GET_OPT(session_handler, Opts) of
        undefined -> undefined;
        {Module, InitOpts} ->
            case Module:init(self(), InitOpts) of
                {ok, State} -> {Module, State};
                {error, _} -> undefined
            end
    end,
    #ssh{
        % ... existing fields ...
        session_handler = SessionHandler
    }.
```

#### After Authentication
```erlang
% In ssh_auth.erl after successful auth
case Ssh#ssh.session_handler of
    {Module, State} ->
        case Module:auth_completed(User, Method, AuthData, State) of
            {ok, NewState} ->
                Ssh#ssh{session_handler = {Module, NewState}};
            {disconnect, Reason} ->
                disconnect(Reason)
        end;
    undefined ->
        Ssh
end
```

### 4. ssh_connection.erl - Channel Hooks

#### Channel Open
```erlang
handle_msg(#ssh_msg_channel_open{channel_type = Type, ...}, Connection, server, SSH) ->
    case SSH#ssh.session_handler of
        {Module, State} ->
            case Module:channel_open(Type, ChannelId, State) of
                {allow, NewState} ->
                    % Proceed with channel open
                    SSH1 = SSH#ssh{session_handler = {Module, NewState}};
                {deny, Reason, NewState} ->
                    % Send channel open failure
                    SSH1 = SSH#ssh{session_handler = {Module, NewState}}
            end;
        undefined ->
            % Normal flow
    end.
```

#### Channel Request (exec, shell, subsystem)
```erlang
handle_msg(#ssh_msg_channel_request{request_type = Type, ...}, Connection, server, SSH) ->
    case SSH#ssh.session_handler of
        {Module, State} ->
            case Module:channel_request(ChannelId, Type, Data, WantReply, State) of
                {allow, NewState} ->
                    SSH1 = SSH#ssh{session_handler = {Module, NewState}},
                    % Proceed with request;
                {deny, NewState} ->
                    SSH1 = SSH#ssh{session_handler = {Module, NewState}},
                    % Send failure
            end;
        undefined ->
            % Normal flow
    end.
```

#### Channel Data
```erlang
handle_msg(#ssh_msg_channel_data{data = Data, ...}, Connection, _, SSH) ->
    case SSH#ssh.session_handler of
        {Module, State} ->
            case Module:channel_data(ChannelId, DataType, Data, State) of
                {ok, NewState} ->
                    SSH1 = SSH#ssh{session_handler = {Module, NewState}},
                    % Proceed normally;
                {throttle, NewState} ->
                    % Pause reading from socket;
                {disconnect, Reason} ->
                    % Disconnect
            end;
        undefined ->
            % Normal flow
    end.
```

### 5. API for Applications

```erlang
% Configure daemon with session handler
ssh:daemon(Port, [
    {session_handler, {my_session_handler, InitOpts}}
]).

% Query session state from channel process
ssh_connection_handler:get_session_state(ConnectionHandler, Key).
```

### 6. Example Implementation

```erlang
-module(git_session_handler).
-behaviour(ssh_session_handler).

-export([init/2, auth_completed/4, channel_request/5]).

init(_ConnRef, _Opts) ->
    {ok, #{repos => [], bytes => 0}}.

auth_completed(User, "publickey", Key, State) ->
    % Look up user permissions based on key
    Repos = lookup_user_repos(User, Key),
    {ok, State#{user => User, repos => Repos}}.

channel_request(ChId, "exec", Cmd, _WantReply, State) ->
    % Parse git command and check permissions
    case parse_git_cmd(Cmd) of
        {ok, Repo, Action} ->
            case is_allowed(State, Repo, Action) of
                true -> {allow, State};
                false -> {deny, State}
            end;
        {error, _} ->
            {deny, State}
    end.
```

## Benefits

1. **Comprehensive Hooks**: Access to all connection lifecycle events
2. **Mutable State**: Session state can be updated throughout connection
3. **Backward Compatible**: Optional feature, defaults to undefined
4. **Clean Separation**: Application logic separate from SSH protocol
5. **Flexible**: Supports all use cases (auth, rate limiting, audit, quotas)

## Migration from auth_context

Old approach (limited):
```erlang
% is_auth_key returns context
{true, #{user => "alice", repos => [...]}}

% Channel init receives it once
init([Shell, Exec], AuthContext) ->
    % Can't update, can't share across channels
```

New approach (comprehensive):
```erlang
% Session handler maintains mutable state
auth_completed(User, Method, Key, State) ->
    {ok, State#{user => User, key => Key}}.

% All events can access and update state
channel_request(ChId, "exec", Cmd, _, State) ->
    % Can check current state, update counters, etc
    {allow, State#{exec_count => maps:get(exec_count, State, 0) + 1}}.
```

## Files to Modify

1. `lib/ssh/src/ssh_session_handler.erl` - NEW behavior module
2. `lib/ssh/src/ssh.hrl` - Update #ssh{} record
3. `lib/ssh/src/ssh_connection_handler.erl` - Initialize session handler
4. `lib/ssh/src/ssh_auth.erl` - Call auth_completed callback
5. `lib/ssh/src/ssh_connection.erl` - Add hooks for channel events
6. `lib/ssh/src/ssh_server_key_api.erl` - Remove auth_context from is_auth_key
7. Remove all auth_context passing code from previous implementation

## Testing

```erlang
% Test with session handler
ssh:daemon(2222, [
    {session_handler, {test_handler, []}},
    {system_dir, "/etc/ssh"}
]).

% Test without (backward compat)
ssh:daemon(2223, [{system_dir, "/etc/ssh"}]).
```
