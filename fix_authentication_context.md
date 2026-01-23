# Fix for Issue #10528: Pass Authentication Context to Channels

## Problem Summary

Git servers need to:
1. Authenticate users by SSH public key
2. Use unified username "git" in URLs
3. Know which real user authenticated to enforce permissions

Currently impossible because `is_auth_key` can only return `true/false`, and channels receive no authentication context.

## Minimal Fix

### 1. Extend is_auth_key Callback (ssh_server_key_api.erl)

```erlang
%% Current:
-callback is_auth_key(PublicKey, User, DaemonOptions) -> boolean().

%% New (backward compatible):
-callback is_auth_key(PublicKey, User, DaemonOptions) -> 
    boolean() | {true, AuthContext :: map()}.
```

### 2. Add auth_context Field to #ssh{} Record (ssh.hrl)

```erlang
-record(ssh, {
    %% ... existing fields ...
    user,
    authenticated = false,
    userauth_banner_sent = false,
    auth_context = #{}  % NEW: Store custom auth data
}).
```

### 3. Store Context in ssh_auth.erl

```erlang
%% In verify_sig/7 and pre_verify_sig/3:
case ssh_transport:call_KeyCb(is_auth_key, [Key, User], Opts) of
    true -> 
        {true, Ssh};
    {true, AuthContext} when is_map(AuthContext) ->
        {true, Ssh#ssh{auth_context = AuthContext}};
    false -> 
        {false, Ssh}
end.
```

### 4. Pass to Connection (ssh_connection.erl)

```erlang
%% In start_cli/2 and start_subsystem/4:
start_cli(#connection{..., ssh_state = SshState}, ChannelId) ->
    AuthContext = SshState#ssh.auth_context,
    ssh_connection_sup:start_channel(..., AuthContext).
```

### 5. Pass to Channel Init (ssh_client_channel.erl)

```erlang
%% Current:
init([Options]) ->
    Cb:init(channel_cb_init_args(Options)).

%% New:
init([Options]) ->
    AuthContext = proplists:get_value(auth_context, Options, #{}),
    Cb:init(channel_cb_init_args(Options), AuthContext).
```

### 6. Update Callback Spec (ssh_server_channel.erl)

```erlang
%% New optional callback (backward compatible):
-callback init(Args :: term(), AuthContext :: map()) ->
    {ok, State} | {stop, Reason}.

%% Old callback still works:
-callback init(Args :: term()) ->
    {ok, State} | {stop, Reason}.
```

## Usage Example

### Server Implementation

```erlang
%% Key handler
-module(git_key_handler).
-behaviour(ssh_server_key_api).

is_auth_key(Key, "git", _Opts) ->
    case lookup_user_by_key(Key) of
        {ok, User} -> 
            {true, #{real_user => User, public_key => Key}};
        error -> 
            false
    end.

%% Exec handler
-module(git_server).
-behaviour(ssh_server_channel).

init([Command], #{real_user := User}) ->
    case parse_git_command(Command) of
        {ok, Operation, Repo} ->
            case check_permission(User, Operation, Repo) of
                true -> 
                    {ok, #state{user = User, command = Command}};
                false -> 
                    {stop, permission_denied}
            end
    end.
```

## Backward Compatibility

- Old callbacks returning `true/false` still work
- Old `init/1` callbacks still work (receive empty map)
- No breaking changes to existing code

## Files to Modify

1. `lib/ssh/src/ssh.hrl` - Add auth_context field
2. `lib/ssh/src/ssh_server_key_api.erl` - Update callback spec
3. `lib/ssh/src/ssh_auth.erl` - Handle new return value, store context
4. `lib/ssh/src/ssh_connection.erl` - Pass context to channel start
5. `lib/ssh/src/ssh_connection_sup.erl` - Thread context through
6. `lib/ssh/src/ssh_channel_sup.erl` - Pass to start_link
7. `lib/ssh/src/ssh_client_channel.erl` - Pass to callback init
8. `lib/ssh/src/ssh_server_channel.erl` - Update callback spec

## Benefits

✅ Enables Git server implementation
✅ Backward compatible
✅ Minimal code changes
✅ Clean architecture
✅ Solves the core problem
