# Authentication Context Feature - Implementation Complete

## Summary
Successfully implemented the ability to pass authentication context from `is_auth_key` callback through to channel initialization, enabling Git server implementations.

## Files Modified

### 1. lib/ssh/src/ssh.hrl
- Added `auth_context = #{}` field to `#ssh{}` record

### 2. lib/ssh/src/ssh_server_key_api.erl  
- Extended `is_auth_key` callback return type to support `{true, AuthContext :: map()}`

### 3. lib/ssh/src/ssh_auth.erl
- Updated `pre_verify_sig/3` to handle new return value and store context
- Updated `verify_sig/7` to handle new return value and store context
- Both functions now return `{true, Ssh}` or `{false, Ssh}` tuples

### 4. lib/ssh/src/ssh_connection.erl
- Updated `start_cli/3` to accept and pass AuthContext
- Updated `start_subsystem/5` to accept and pass AuthContext
- Updated `handle_cli_msg/4` to accept and pass AuthContext
- Updated all call sites (pty-req, shell, exec, env, subsystem) to extract and pass `SSH#ssh.auth_context`

### 5. lib/ssh/src/ssh_connection_sup.erl
- Updated `start_channel/9` (was /8) to accept and pass AuthContext

### 6. lib/ssh/src/ssh_channel_sup.erl
- Updated `start_child/9` (was /8) to accept and pass AuthContext
- Updated `start_the_channel/8` (was /7) to accept and pass AuthContext

### 7. lib/ssh/src/ssh_client_channel.erl
- Added `start_link/6` that accepts AuthContext
- Updated `init/1` to:
  - Extract AuthContext from options
  - Try calling `Callback:init/2` with AuthContext first
  - Fall back to `Callback:init/1` for backward compatibility

### 8. lib/ssh/src/ssh_server_channel.erl
- Added optional `-callback init/2` that receives AuthContext
- Added `-optional_callbacks([init/2])`
- Documented the new callback

## Usage Example

```erlang
%% Key handler
-module(git_key_handler).
-behaviour(ssh_server_key_api).

-export([host_key/2, is_auth_key/3]).

host_key(Algorithm, DaemonOptions) ->
    ssh_file:host_key(Algorithm, DaemonOptions).

is_auth_key(Key, "git", _Opts) ->
    case lookup_user_by_key(Key) of
        {ok, User} -> 
            {true, #{real_user => User, public_key => Key}};
        error -> 
            false
    end.

%% Channel callback
-module(git_server).
-behaviour(ssh_server_channel).

-export([init/2, handle_msg/2, handle_ssh_msg/2, terminate/2]).

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

%% Start daemon
ssh:daemon(2222, [
    {exec, {git_server, []}},
    {key_cb, {git_key_handler, []}}
]).
```

## Backward Compatibility

✅ Old `is_auth_key/3` returning `true/false` still works
✅ Old `init/1` callbacks still work (automatic fallback)
✅ No breaking changes to existing code

## Benefits

✅ Enables Git server implementation with unified URLs
✅ Clean architecture - context flows naturally through the stack
✅ Fully backward compatible
✅ Minimal code changes
✅ Solves the core problem described in issue #10528
