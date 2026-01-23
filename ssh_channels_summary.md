# SSH Channel Behaviors: External API and the Authentication Problem

## Summary

**ssh_server_channel** and **ssh_client_channel** are **PUBLIC, EXTERNAL APIs** designed for users to implement custom SSH services. They are NOT internal implementation details.

## What They're For

### ssh_server_channel
Implement server-side SSH services:
- **Subsystems** - Named services like SFTP (e.g., `ssh user@host -s mysubsystem`)
- **Exec handlers** - Command execution (e.g., `ssh user@host mycommand`)
- **Custom shells** - Interactive CLIs

### ssh_client_channel
Implement client-side channel handlers (less common, mostly for advanced use cases)

## Real-World Examples in OTP

1. **ssh_sftpd** - SFTP server (uses ssh_server_channel)
2. **ssh_sample_cli** - Custom CLI example in examples/
3. **ssh_cli** - Default Erlang shell over SSH

## How They Work

```
User connects via SSH
    ↓
Authentication (is_auth_key callback)
    ↓
User requests subsystem/exec/shell
    ↓
SSH daemon spawns channel process
    ↓
Your ssh_server_channel:init/1 called
    ↓
Your handle_ssh_msg/2 handles data/commands
```

## The Architecture Problem for Git Servers

### What Git Servers Need

```
1. User connects: ssh://git@server/repo.git
2. Authenticate by PUBLIC KEY (not username)
3. Determine real user from key: "alice"
4. Check if alice can access repo.git
5. Execute git command with alice's permissions
```

### What's Currently Possible

```
1. User connects: ssh://git@server/repo.git
2. is_auth_key(Key, "git", Opts) -> true/false
   - Can identify user here: "alice"
   - Can ONLY return true/false
   - CANNOT pass "alice" to next step
3. Channel init([Command]) called
   - Knows command: "git-upload-pack '/repo.git'"
   - Knows SSH user: "git"
   - DOES NOT KNOW real user: "alice"
4. Cannot check permissions properly!
```

### The Missing Link

```erlang
%% Current API:
-callback is_auth_key(Key, User, Opts) -> boolean().
-callback init(Args) -> {ok, State}.

%% Needed API:
-callback is_auth_key(Key, User, Opts, ConnState) -> 
    {true, NewConnState} | {false, ConnState}.
-callback init(Args, ConnectionState) -> {ok, State}.
```

## Why Workarounds Fail

### Workaround 1: Store in ETS
```erlang
%% In is_auth_key:
ets:insert(users, {ConnectionPid, "alice"})

%% In channel init:
User = ets:lookup(users, ???)  % What PID? Channel ≠ Connection!
```
**Problem**: Channel process and connection handler are different processes.

### Workaround 2: Unique usernames
```
ssh://alice@server/repo.git  % Instead of git@server
```
**Problem**: Poor UX, not standard Git convention, exposes user identities.

### Workaround 3: Encode in path
```
ssh://git@server/alice/repo.git  % Instead of /repo.git
```
**Problem**: Still can't distinguish repo owner from accessor, non-standard.

## Code Examples

See the created files:
- `/tmp/simple_echo_server.erl` - Working echo server example
- `/tmp/git_like_server.erl` - Shows the authentication problem
- `/tmp/ssh_channel_examples.md` - Comprehensive guide

## Relation to Issue #10528

The issue requests the ability to **pass state from authentication through to channel handlers**. This would enable:

1. Authenticate user by public key in `is_auth_key`
2. Return `{true, #{real_user => "alice"}}`
3. Channel `init` receives this state
4. Channel can enforce per-user permissions

Without this, implementing standard Git server semantics (unified git@ URLs with key-based identity) is architecturally impossible in Erlang/OTP SSH.

## Key Takeaway

**ssh_server_channel and ssh_client_channel are external APIs for building custom SSH services**, but they lack the ability to receive authentication context, making certain use cases (like Git servers) impossible to implement properly.
