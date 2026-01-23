# Implementation Plan for Authentication Context Feature

## Summary
Add `auth_context` field to pass authentication data from `is_auth_key` to channel callbacks.

## Changes Required

### 1. ssh.hrl - Add field to #ssh{} record (line ~1280)

```erlang
-record(ssh, {
    %% ... existing fields ...
    authenticated = false,
    userauth_banner_sent = false,
    auth_context = #{}  % NEW: Custom authentication context
}).
```

### 2. ssh_server_key_api.erl - Extend callback spec (line ~71)

```erlang
%% OLD:
-callback is_auth_key(PublicKey, User, DaemonOptions) -> boolean().

%% NEW (backward compatible):
-callback is_auth_key(PublicKey, User, DaemonOptions) -> 
    boolean() | {true, AuthContext :: map()}.
```

### 3. ssh_auth.erl - Handle new return value (line ~554, ~567)

```erlang
%% In pre_verify_sig/3 and verify_sig/7:
case ssh_transport:call_KeyCb(is_auth_key, [Key, User], Opts) of
    true -> 
        {true, Ssh};
    {true, AuthContext} when is_map(AuthContext) ->
        {true, Ssh#ssh{auth_context = AuthContext}};
    false -> 
        {false, Ssh}
end.
```

### 4. ssh_connection.erl - Pass context to channel start

```erlang
%% In start_cli/2 (line ~1494):
start_cli(#connection{ssh_state = SshState, ...}, ChannelId) ->
    AuthContext = SshState#ssh.auth_context,
    ssh_connection_sup:start_channel(..., AuthContext).

%% In start_subsystem/4 (line ~1505):
start_subsystem(..., #connection{ssh_state = SshState}) ->
    AuthContext = SshState#ssh.auth_context,
    ssh_connection_sup:start_channel(..., AuthContext).
```

### 5. ssh_connection_sup.erl - Thread context (line ~53)

```erlang
start_channel(Role, SupPid, ConnRef, Callback, Id, Args, Exec, Opts, AuthContext) ->
    ssh_channel_sup:start_child(Role, ..., AuthContext).
```

### 6. ssh_channel_sup.erl - Pass to start_link (line ~81)

```erlang
start_the_channel(ChanMod, ..., AuthContext) ->
    ChildSpec = #{
        start => {ChanMod, start_link, [..., AuthContext]}
    }.
```

### 7. ssh_client_channel.erl - Pass to callback init (line ~368)

```erlang
init([Options]) ->
    AuthContext = proplists:get_value(auth_context, Options, #{}),
    case Cb:init(channel_cb_init_args(Options), AuthContext) of
        {ok, ChannelState} -> ...
    catch
        error:undef -> 
            %% Fallback to old init/1 for backward compatibility
            Cb:init(channel_cb_init_args(Options))
    end.
```

### 8. ssh_server_channel.erl - Update callback spec

```erlang
%% Add optional callback (backward compatible):
-callback init(Args :: term(), AuthContext :: map()) ->
    {ok, State} | {stop, Reason}.
```

## Backward Compatibility

- Old `is_auth_key` returning `true/false` still works
- Old `init/1` callbacks still work (receive empty map via try/catch)
- No breaking changes

## Usage Example

```erlang
%% Key handler
is_auth_key(Key, "git", _Opts) ->
    case lookup_user_by_key(Key) of
        {ok, User} -> {true, #{real_user => User}};
        error -> false
    end.

%% Channel callback
init([Command], #{real_user := User}) ->
    case check_permission(User, Command) of
        true -> {ok, #state{user = User}};
        false -> {stop, permission_denied}
    end.
```

## Files to Modify

1. lib/ssh/src/ssh.hrl
2. lib/ssh/src/ssh_server_key_api.erl
3. lib/ssh/src/ssh_auth.erl
4. lib/ssh/src/ssh_connection.erl
5. lib/ssh/src/ssh_connection_sup.erl
6. lib/ssh/src/ssh_channel_sup.erl
7. lib/ssh/src/ssh_client_channel.erl
8. lib/ssh/src/ssh_server_channel.erl

## Testing

Create test cases for:
- Old-style callbacks (backward compatibility)
- New-style callbacks with auth context
- Git server use case
