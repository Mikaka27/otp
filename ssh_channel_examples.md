# SSH Channel Behaviors - Usage Guide

## Are They External or Internal?

**EXTERNAL** - These behaviors are **designed for users** to implement custom SSH services. They are public APIs documented in the SSH application.

- `ssh_server_channel` - For implementing SSH server subsystems/services
- `ssh_client_channel` - For implementing SSH client subsystems/services

## Example 1: Simple Echo Server (Server-Side Subsystem)

```erlang
-module(echo_server).
-behaviour(ssh_server_channel).

-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-record(state, {
    cm,           % Connection manager
    channel_id    % Channel ID
}).

%% Called when subsystem is started
init([]) ->
    {ok, #state{}}.

%% Handle SSH channel up message (first message received)
handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State) ->
    io:format("Echo server started on channel ~p~n", [ChannelId]),
    {ok, State#state{cm = ConnectionManager, channel_id = ChannelId}}.

%% Handle SSH protocol messages
handle_ssh_msg({ssh_cm, CM, {data, Ch, 0, Data}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    %% Echo data back to client
    ssh_connection:send(CM, Ch, Data),
    {ok, State};

handle_ssh_msg({ssh_cm, CM, {eof, Ch}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, CM, {signal, Ch, _Signal}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, CM, {exit_signal, Ch, _, _, _}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    {stop, Ch, State};

handle_ssh_msg({ssh_cm, CM, {exit_status, Ch, _Status}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    {stop, Ch, State}.

terminate(_Reason, _State) ->
    ok.
```

### Starting the Echo Server

```erlang
%% Start SSH daemon with echo subsystem
start_daemon() ->
    ssh:start(),
    ssh:daemon(2222, [
        {subsystems, [{"echo", {echo_server, []}}]},
        {system_dir, "/path/to/host/keys"},
        {user_dir, "/path/to/user/keys"}
    ]).

%% Client connects with:
%% ssh -p 2222 user@localhost -s echo
```

## Example 2: Simple Command Executor (Exec Handler)

```erlang
-module(cmd_executor).
-behaviour(ssh_server_channel).

-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-record(state, {cm, channel_id, cmd}).

%% Init receives the exec command as last argument
init([Cmd]) ->
    {ok, #state{cmd = Cmd}}.

handle_msg({ssh_channel_up, ChannelId, CM}, State) ->
    %% Execute command when channel is ready
    spawn(fun() -> execute_cmd(CM, ChannelId, State#state.cmd) end),
    {ok, State#state{cm = CM, channel_id = ChannelId}}.

handle_ssh_msg({ssh_cm, CM, {eof, Ch}}, #state{cm = CM, channel_id = Ch} = State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, CM, {signal, Ch, _}}, #state{cm = CM, channel_id = Ch} = State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, CM, {exit_status, Ch, _}}, #state{cm = CM, channel_id = Ch} = State) ->
    {stop, Ch, State}.

terminate(_Reason, _State) ->
    ok.

execute_cmd(CM, Ch, Cmd) ->
    %% Simple command execution
    Result = case Cmd of
        "date" -> os:cmd("date");
        "uptime" -> os:cmd("uptime");
        _ -> "Unknown command\n"
    end,
    ssh_connection:send(CM, Ch, Result),
    ssh_connection:send_eof(CM, Ch),
    ssh_connection:exit_status(CM, Ch, 0).
```

### Starting with Exec Handler

```erlang
start_daemon() ->
    ssh:daemon(2222, [
        {exec, {cmd_executor, []}},
        {system_dir, "/path/to/host/keys"},
        {user_dir, "/path/to/user/keys"}
    ]).

%% Client runs:
%% ssh -p 2222 user@localhost date
```

## Example 3: Git-Like Server (Shows the Problem)

```erlang
-module(git_server).
-behaviour(ssh_server_channel).

-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-record(state, {cm, channel_id, user}).

%% THIS IS THE PROBLEM: We only get the SSH username, not the real user!
init([Cmd]) ->
    %% Cmd might be: "git-upload-pack '/repo.git'"
    %% But we don't know which user authenticated!
    {ok, #state{}}.

handle_msg({ssh_channel_up, ChannelId, CM}, State) ->
    %% PROBLEM: How do we know which public key was used?
    %% We only know the SSH username was "git"
    
    %% We need to check permissions, but we can't!
    %% check_permission(???, Repo) - who is the user?
    
    {ok, State#state{cm = CM, channel_id = ChannelId}}.

handle_ssh_msg({ssh_cm, CM, {data, Ch, 0, Data}}, State) ->
    %% Handle git protocol data
    %% But still don't know the real user for authorization
    {ok, State};

handle_ssh_msg({ssh_cm, _, {eof, Ch}}, State) ->
    {stop, Ch, State}.

terminate(_Reason, _State) ->
    ok.
```

## Example 4: Client-Side Channel

```erlang
-module(my_client_channel).
-behaviour(ssh_client_channel).

-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2,
         handle_call/3, handle_cast/2, code_change/3]).

-record(state, {cm, channel_id}).

init([]) ->
    {ok, #state{}}.

handle_msg({ssh_channel_up, ChannelId, CM}, State) ->
    %% Channel is ready, can start sending data
    {ok, State#state{cm = CM, channel_id = ChannelId}}.

handle_ssh_msg({ssh_cm, CM, {data, Ch, 0, Data}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    io:format("Received: ~s~n", [Data]),
    {ok, State};

handle_ssh_msg({ssh_cm, CM, {eof, Ch}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    {stop, Ch, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
```

### Using the Client Channel

```erlang
use_client() ->
    ssh:start(),
    {ok, Conn} = ssh:connect("localhost", 2222, [
        {user, "myuser"},
        {password, "mypass"}
    ]),
    
    %% Open a channel
    {ok, ChannelId} = ssh_connection:session_channel(Conn, infinity),
    
    %% Start our client channel behavior
    {ok, ChannelPid} = ssh_client_channel:start_link(
        Conn, ChannelId, my_client_channel, []
    ),
    
    %% Request subsystem
    success = ssh_connection:subsystem(Conn, ChannelId, "echo", infinity),
    
    %% Send data
    ssh_connection:send(Conn, ChannelId, "Hello\n"),
    
    timer:sleep(1000),
    ssh:close(Conn).
```

## Key Points

1. **ssh_server_channel** - Implement server-side subsystems, exec handlers, or shells
2. **ssh_client_channel** - Implement client-side channel handlers (less common)
3. **They are PUBLIC APIs** - Meant for external use
4. **The Problem**: No way to pass authentication context (like which public key) to the channel

## Common Use Cases

- **SFTP alternatives** - Custom file transfer protocols
- **Custom shells** - Application-specific CLIs (like the sample_cli example)
- **RPC systems** - Remote procedure call over SSH
- **Git servers** - Would work if authentication context was available
- **Database tunnels** - Custom protocol handlers
- **Monitoring agents** - Secure command execution

## The Missing Piece

What's needed for Git-like servers:

```erlang
%% Hypothetical enhanced API:
init([Cmd, ConnectionState]) ->
    %% ConnectionState would contain:
    %% #{authenticated_user => "alice", 
    %%   public_key => Key,
    %%   peer_address => {192,168,1,100}}
    {ok, #state{real_user = maps:get(authenticated_user, ConnectionState)}}.
```

This would allow proper per-user authorization in subsystems and exec handlers.
