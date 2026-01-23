# Git Servers and SSH: Exec Handlers, Not Subsystems

## Short Answer

**Git servers use EXEC handlers, not subsystems or channels directly.**

When you run:
```bash
git clone ssh://git@server/repo.git
```

Git actually executes:
```bash
ssh git@server "git-upload-pack '/repo.git'"
```

This is an **exec request**, not a subsystem request.

## How Git Uses SSH

### Git Commands Over SSH

```bash
# Clone/fetch (read)
git clone ssh://git@server/repo.git
→ ssh git@server "git-upload-pack '/repo.git'"

# Push (write)  
git push ssh://git@server/repo.git
→ ssh git@server "git-receive-pack '/repo.git'"
```

### SSH Protocol Flow

```
1. Client connects: ssh git@server
2. Client authenticates (public key)
3. Client sends exec request: "git-upload-pack '/repo.git'"
4. Server handles exec request
5. Server spawns channel with exec handler
6. Git protocol runs over the channel
```

## Code Flow for Git Server

### 1. Exec Request Received (ssh_connection.erl:1199)

```erlang
handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
                                    request_type = "exec",
                                    want_reply = WantReply,
                                    data = Data},
           Connection, server, _SSH) ->
    <<?DEC_BIN(Command, _Len)>> = Data,
    %% Command = "git-upload-pack '/repo.git'"
    handle_cli_msg(Connection, ChannelId,
                   {exec, ChannelId, WantReply, binary_to_list(Command)}).
```

### 2. Start CLI/Exec Handler (ssh_connection.erl:1494)

```erlang
start_cli(#connection{options = Options, 
                      cli_spec = CliSpec,
                      exec = Exec,  % ← Your exec handler
                      connection_supervisor = ConnectionSup}, ChannelId) ->
    case CliSpec of
        no_cli ->
            {error, cli_disabled};
        {CbModule, Args} ->
            %% Exec is passed as the last parameter
            ssh_connection_sup:start_channel(server, ConnectionSup, self(), 
                                            CbModule, ChannelId, Args, 
                                            Exec, Options)
    end.
```

### 3. Your Callback Receives Command (ssh_client_channel.erl:368)

```erlang
channel_cb_init_args(Options) ->
    case proplists:get_value(exec, Options) of
        undefined ->
            proplists:get_value(init_args, Options);  % Subsystem
        Exec ->
            proplists:get_value(init_args, Options) ++ [Exec]  % Exec handler
            %% Exec = "git-upload-pack '/repo.git'"
    end.
```

Your callback receives:
```erlang
init([Command]) ->
    %% Command = "git-upload-pack '/repo.git'"
    {ok, #state{command = Command}}.
```

## Configuring Git Server

### Option 1: Single Exec Handler (Most Common)

```erlang
ssh:daemon(2222, [
    {exec, {git_server, []}},  % All exec commands go here
    {system_dir, "/path/to/host/keys"},
    {key_cb, {git_key_handler, []}}
]).
```

Your handler:
```erlang
-module(git_server).
-behaviour(ssh_server_channel).

init([Command]) ->
    %% Parse command: "git-upload-pack '/repo.git'"
    case parse_git_command(Command) of
        {ok, Operation, RepoPath} ->
            %% THE PROBLEM: Don't know which user authenticated!
            {ok, #state{operation = Operation, repo = RepoPath}};
        {error, _} ->
            {stop, invalid_command}
    end.
```

### Option 2: Shell with Command Parsing

```erlang
ssh:daemon(2222, [
    {shell, fun(User, _Peer) -> 
        spawn(fun() -> git_shell(User) end)
    end}
]).

git_shell(User) ->
    %% User is just "git", not the real user!
    case io:get_line("") of
        eof -> ok;
        Command -> 
            handle_git_command(Command),
            git_shell(User)
    end.
```

## The Authentication Problem for Git Servers

### What Happens

```
1. User runs: git clone ssh://git@server/repo.git

2. SSH authenticates with public key:
   is_auth_key(Key, "git", Opts) ->
       RealUser = lookup_user_by_key(Key),  % "alice"
       true.  % Can only return true/false!

3. Git sends exec request:
   "git-upload-pack '/repo.git'"

4. Your exec handler receives:
   init(["git-upload-pack '/repo.git'"]) ->
       %% Know: command = "git-upload-pack '/repo.git'"
       %% Know: SSH user = "git"
       %% DON'T KNOW: real user = "alice"
       %% Can't check if alice has access to repo.git!
```

### Why This Matters

Git servers need to:
1. Accept all users as "git@server" (standard convention)
2. Identify real user by their SSH public key
3. Check permissions: Can this user access this repo?
4. Execute git command with appropriate permissions

**Currently impossible** because authentication context is lost.

## Real-World Git Server Implementations

### GitHub/GitLab Architecture

They DON'T use Erlang SSH for this reason. They use:
- Custom SSH servers (Go, Rust, C)
- Or workarounds like unique usernames per user

### What They Need

```erlang
%% Hypothetical working implementation:

%% In is_auth_key:
is_auth_key(Key, "git", Opts, ConnState) ->
    RealUser = lookup_user_by_key(Key),
    {true, ConnState#{real_user => RealUser}}.

%% In exec handler:
init([Command], #{real_user := RealUser}) ->
    case parse_git_command(Command) of
        {ok, Operation, RepoPath} ->
            case check_permission(RealUser, Operation, RepoPath) of
                true -> 
                    execute_git(Operation, RepoPath, RealUser),
                    {ok, #state{}};
                false ->
                    {stop, permission_denied}
            end
    end.
```

## Comparison: Subsystems vs Exec

| Feature | Subsystem | Exec Handler |
|---------|-----------|--------------|
| Invocation | `ssh user@host -s subsystem` | `ssh user@host "command"` |
| Use case | Named services (SFTP) | Command execution |
| Git usage | ❌ Not used | ✅ Used |
| Config | `{subsystems, [...]}` | `{exec, {Module, []}}` |
| Init args | `init(Args)` | `init(Args ++ [Command])` |

## Summary

1. **Git servers use exec handlers**, not subsystems
2. Git runs commands like `git-upload-pack` via SSH exec
3. The exec handler receives the command as the last init argument
4. **The same authentication problem exists**: no way to know which user authenticated
5. This is why implementing a proper Git server in Erlang/OTP SSH is currently impossible

The requested feature (passing authentication state) would fix this for both subsystems AND exec handlers.
