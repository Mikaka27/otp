# SSH Channel Lifecycle: Exec vs Subsystems vs Shell

## Your Question is Spot On!

You're correct that **for exec handlers, one channel = one command execution**.

## Three Different Channel Types

### 1. Exec Handler (Git uses this)

**Lifecycle**: One command, then close

```
Client: ssh user@host "command"
    ↓
Server: Opens channel
    ↓
Server: Executes command
    ↓
Command: Runs, produces output
    ↓
Command: Exits
    ↓
Server: Closes channel
    ↓
Connection: May stay open or close
```

**Example - Git Clone:**
```bash
git clone ssh://git@server/repo.git

# What actually happens:
ssh git@server "git-upload-pack '/repo.git'"
  ↓ Opens channel
  ↓ Executes git-upload-pack
  ↓ Git protocol data flows
  ↓ Command completes
  ↓ Channel closes
```

**Code:**
```erlang
init([Command]) ->
    %% Command = "git-upload-pack '/repo.git'"
    {ok, #state{command = Command}}.

handle_msg({ssh_channel_up, ChannelId, CM}, State) ->
    %% Execute the command immediately
    spawn_git_process(CM, ChannelId, State#state.command),
    {ok, State#state{cm = CM, channel_id = ChannelId}}.

handle_ssh_msg({ssh_cm, CM, {data, Ch, 0, Data}}, State) ->
    %% Git protocol data from the spawned process
    {ok, State}.

handle_ssh_msg({ssh_cm, CM, {exit_status, Ch, _}}, State) ->
    %% Command finished, close channel
    {stop, Ch, State}.
```

### 2. Subsystem (SFTP uses this)

**Lifecycle**: Long-lived, handles multiple requests

```
Client: ssh user@host -s sftp
    ↓
Server: Opens channel
    ↓
Server: Starts subsystem
    ↓
Subsystem: Handles request 1
Subsystem: Handles request 2
Subsystem: Handles request 3
    ...
    ↓
Client: Closes connection
    ↓
Server: Closes channel
```

**Example - SFTP Session:**
```bash
sftp user@host
sftp> ls          # Request 1
sftp> get file    # Request 2
sftp> put file    # Request 3
sftp> quit        # Close
```

**Code:**
```erlang
init([]) ->
    {ok, #state{}}.

handle_msg({ssh_channel_up, ChannelId, CM}, State) ->
    %% Subsystem is ready, wait for requests
    {ok, State#state{cm = CM, channel_id = ChannelId}}.

handle_ssh_msg({ssh_cm, CM, {data, Ch, 0, Data}}, State) ->
    %% SFTP protocol request
    Response = handle_sftp_request(Data, State),
    ssh_connection:send(CM, Ch, Response),
    {ok, State}.  % Keep channel open for more requests

handle_ssh_msg({ssh_cm, CM, {eof, Ch}}, State) ->
    %% Client closed, stop subsystem
    {stop, Ch, State}.
```

### 3. Shell (Interactive)

**Lifecycle**: Long-lived, interactive

```
Client: ssh user@host
    ↓
Server: Opens channel
    ↓
Server: Starts shell
    ↓
Shell: Displays prompt
User: Types command 1
Shell: Executes, displays output
User: Types command 2
Shell: Executes, displays output
    ...
    ↓
User: Types "exit"
    ↓
Server: Closes channel
```

## Git Server: One Command Per Channel

### Why Git Works This Way

```bash
# Clone operation
git clone ssh://git@server/repo.git
  → Opens connection
  → Opens channel
  → Executes: git-upload-pack '/repo.git'
  → Git protocol runs
  → Channel closes
  → Connection closes

# Later: Push operation
git push
  → Opens NEW connection
  → Opens NEW channel
  → Executes: git-receive-pack '/repo.git'
  → Git protocol runs
  → Channel closes
  → Connection closes
```

Each Git operation is **completely independent**:
- New SSH connection
- New authentication
- New channel
- One command
- Close everything

### Correct Implementation Pattern

```erlang
-module(git_server).
-behaviour(ssh_server_channel).

-record(state, {
    cm,
    channel_id,
    command,        % The ONE command for this channel
    git_port        % Port to spawned git process
}).

init([Command]) ->
    %% Save the command, will execute when channel is up
    {ok, #state{command = Command}}.

handle_msg({ssh_channel_up, ChannelId, CM}, State) ->
    %% Channel is ready, execute the command NOW
    case parse_git_command(State#state.command) of
        {ok, Operation, RepoPath} ->
            %% Check permissions (if we knew the user!)
            %% Spawn git-upload-pack or git-receive-pack
            Port = spawn_git_command(Operation, RepoPath),
            {ok, State#state{
                cm = CM, 
                channel_id = ChannelId,
                git_port = Port
            }};
        {error, _} ->
            send_error(CM, ChannelId, "Invalid command"),
            {stop, ChannelId, State}
    end.

handle_ssh_msg({ssh_cm, CM, {data, Ch, 0, Data}}, State) ->
    %% Data from client (Git protocol)
    %% Send to spawned git process
    port_command(State#state.git_port, Data),
    {ok, State}.

handle_info({Port, {data, Data}}, #state{git_port = Port} = State) ->
    %% Data from git process
    %% Send to client
    ssh_connection:send(State#state.cm, State#state.channel_id, Data),
    {ok, State}.

handle_info({Port, {exit_status, Status}}, #state{git_port = Port} = State) ->
    %% Git command finished
    ssh_connection:exit_status(State#state.cm, State#state.channel_id, Status),
    {stop, State#state.channel_id, State}.
```

## Why You Don't Need to Handle Multiple Commands

For exec handlers:
- ✅ One channel = One command
- ✅ Command is passed in init
- ✅ Execute immediately when channel is up
- ✅ Channel closes when command completes
- ❌ No need to parse multiple commands
- ❌ No need to maintain command queue

For subsystems (like SFTP):
- ✅ One channel = Multiple requests
- ✅ Channel stays open
- ✅ Parse each data message as a request
- ✅ Send responses back
- ✅ Close when client disconnects

## Comparison Table

| Feature | Exec | Subsystem | Shell |
|---------|------|-----------|-------|
| Commands per channel | 1 | Many | Many |
| Invocation | `ssh host "cmd"` | `ssh host -s name` | `ssh host` |
| Command in init | Yes | No | No |
| Channel lifetime | Short | Long | Long |
| Git usage | ✅ Yes | ❌ No | ❌ No |
| SFTP usage | ❌ No | ✅ Yes | ❌ No |
| Interactive | ❌ No | ❌ No | ✅ Yes |

## Summary

You were absolutely right to question this! 

For Git servers (exec handlers):
1. **One channel = One command** (passed in init)
2. Execute the command when channel comes up
3. Handle data flow between client and spawned process
4. Close channel when command completes
5. **No need to handle multiple commands per channel**

The authentication problem still applies: you need to know which user authenticated to check permissions before executing the command.
