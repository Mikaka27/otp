# Where Subsystem Callback Modules Are Called in SSH Code

## Call Chain Overview

```
Client sends subsystem request
    ↓
ssh_connection.erl: handle_msg/4 (line ~1132)
    ↓
ssh_connection.erl: start_subsystem/4 (line ~1505)
    ↓
ssh_connection_sup.erl: start_channel/8 (line ~53)
    ↓
ssh_channel_sup.erl: start_child/8 (line ~48)
    ↓
ssh_channel_sup.erl: start_the_channel/7 (line ~81)
    ↓
ssh_server_channel.erl: start_link/5 (delegates to ssh_client_channel)
    ↓
ssh_client_channel.erl: start_link/5 (line ~327)
    ↓
ssh_client_channel.erl: init/1 (line ~368)
    ↓
YOUR_CALLBACK_MODULE:init/1  ← YOUR CODE RUNS HERE
```

## Detailed Code Flow

### 1. Subsystem Request Received (ssh_connection.erl:1132)

```erlang
handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
                                    request_type = "subsystem",
                                    want_reply = WantReply,
                                    data = Data},
           #connection{channel_cache = Cache} = Connection, server, _SSH) ->
    <<?DEC_BIN(SsName,_SsLen)>> = Data,
    #channel{remote_id=RemoteId} = Channel = 
        ssh_client_channel:cache_lookup(Cache, ChannelId), 
    Reply =
        case start_subsystem(SsName, Connection, Channel,
                             {subsystem, ChannelId, WantReply, binary_to_list(SsName)}) of
            {ok, Pid} ->
                erlang:monitor(process, Pid),
                ssh_client_channel:cache_update(Cache, Channel#channel{user=Pid}),
                channel_success_msg(RemoteId);
            {error,_Error} ->
                channel_failure_msg(RemoteId)
        end,
    {[{connection_reply,Reply}], Connection}.
```

**Key Point**: At this stage, we have:
- `ChannelId` - the channel ID
- `SsName` - subsystem name (e.g., "echo", "sftp")
- `Connection` - connection state (includes SSH user, but NOT the real authenticated user)

### 2. Start Subsystem (ssh_connection.erl:1505)

```erlang
start_subsystem(BinName, #connection{options = Options,
                                     connection_supervisor = ConnectionSup},
                #channel{local_id = ChannelId}, _ReplyMsg) ->
    Name = binary_to_list(BinName),
    case check_subsystem(Name, Options) of
        {Callback, Opts} when is_atom(Callback), Callback =/= none ->
            ssh_connection_sup:start_channel(server, ConnectionSup, self(), 
                                            Callback, ChannelId, Opts, 
                                            undefined, Options);
        {none, _} ->
            {error, bad_subsystem};
        {_, _} ->
            {error, legacy_option_not_supported}
    end.
```

**Key Point**: 
- `Callback` is YOUR callback module (e.g., `echo_server`)
- `Opts` are the init args you configured
- `undefined` is the Exec parameter (only used for exec handlers)

### 3. Check Subsystem Configuration (ssh_connection.erl:1520)

```erlang
check_subsystem(SsName, Options) ->
    Subsystems = ?GET_OPT(subsystems, Options),
    case proplists:get_value(SsName, Subsystems, {none, []}) of
        Fun when is_function(Fun) ->
            {Fun, []};
        {_, _} = Value ->
            Value
    end.
```

This looks up the subsystem from daemon options:
```erlang
ssh:daemon(2222, [
    {subsystems, [{"echo", {echo_server, [arg1, arg2]}}]}
    %                      ^^^^^^^^^^^^  ^^^^^^^^^^^^
    %                      Callback      Opts
]).
```

### 4. Start Channel Process (ssh_channel_sup.erl:48)

```erlang
start_child(server, ChannelSup, ConnRef, Callback, Id, Args, Exec, Opts) 
        when is_pid(ConnRef) ->
    case max_num_channels_not_exceeded(ChannelSup, Opts) of
        true ->
            start_the_channel(ssh_server_channel, ChannelSup, ConnRef, 
                             Callback, Id, Args, Exec);
        false ->
            {error, max_num_channels_exceeded}
    end.
```

### 5. Actually Spawn the Process (ssh_channel_sup.erl:81)

```erlang
start_the_channel(ChanMod, ChannelSup, ConnRef, Callback, Id, Args, Exec) ->
    ChildSpec =
        #{id       => make_ref(),
          start    => {ChanMod, start_link, [ConnRef, Id, Callback, Args, Exec]},
          %                                  ^^^^^^^  ^^  ^^^^^^^^  ^^^^  ^^^^
          %                                  Conn Mgr |  Your Mod  Your  Exec
          %                                           |           Args   Cmd
          %                                      Channel ID
          restart  => temporary,
          type     => worker,
          modules  => [ChanMod]
         },
    supervisor:start_child(ChannelSup, ChildSpec).
```

**This is where your callback module process is spawned!**

### 6. Channel Behavior Init (ssh_client_channel.erl:368)

```erlang
init([Options]) ->    
    Cb = proplists:get_value(channel_cb, Options),
    ConnectionManager =  proplists:get_value(cm, Options),
    ChannelId = proplists:get_value(channel_id, Options),
    process_flag(trap_exit, true),
    try Cb:init(channel_cb_init_args(Options)) of
        {ok, ChannelState} ->
            State = #state{cm = ConnectionManager, 
                          channel_cb = Cb,
                          channel_id = ChannelId,
                          channel_state = ChannelState},
            self() ! {ssh_channel_up, ChannelId, ConnectionManager}, 
            {ok, State};
        % ... error cases
    end.

channel_cb_init_args(Options) ->
    case proplists:get_value(exec, Options) of
        undefined ->
            proplists:get_value(init_args, Options);  % For subsystems
        Exec ->
            proplists:get_value(init_args, Options) ++ [Exec]  % For exec
    end.
```

**YOUR CALLBACK MODULE'S init/1 IS CALLED HERE!**

For subsystems: `YourModule:init(Args)`
For exec: `YourModule:init(Args ++ [Command])`

## The Problem: No Authentication Context

Notice what's passed to your callback:
- ✅ Channel ID
- ✅ Connection Manager reference
- ✅ Your configured init args
- ✅ Exec command (if exec handler)
- ❌ **NO authenticated user identity**
- ❌ **NO public key used**
- ❌ **NO custom authentication state**

The connection state exists in `ssh_connection.erl` but contains only:
```erlang
#connection{
    options = Options,        % Daemon options
    cli_spec = CliSpec,       % Shell/CLI spec
    exec = Exec,              % Exec handler spec
    % ... but NO custom auth state
}
```

The SSH state in `ssh_connection_handler.erl` has:
```erlang
#ssh{
    user = "git",             % SSH protocol username
    % ... but NO real user identity from is_auth_key
}
```

## What Would Be Needed

To pass authentication context, the flow would need to be:

```erlang
% In is_auth_key (ssh_auth.erl:554):
is_auth_key(Key, User, Opts, ConnState) ->
    RealUser = lookup_user_by_key(Key),
    {true, ConnState#{real_user => RealUser}}.

% Store in #ssh{} record:
#ssh{
    user = "git",
    auth_context = #{real_user => "alice"}  % NEW FIELD
}

% Pass through start_subsystem:
start_subsystem(BinName, Connection, Channel, ReplyMsg) ->
    AuthContext = Connection#connection.auth_context,
    ssh_connection_sup:start_channel(..., AuthContext).

% Pass to channel init:
init([Options, AuthContext]) ->
    Cb:init(channel_cb_init_args(Options), AuthContext).

% Your callback receives it:
init(Args, #{real_user := "alice"}) ->
    {ok, #state{user = "alice"}}.
```

## Summary

Subsystem callbacks are invoked at:
- **File**: `ssh_client_channel.erl`
- **Function**: `init/1` (line ~368)
- **Call**: `Cb:init(channel_cb_init_args(Options))`

The problem is that by this point, all authentication context has been lost, making it impossible to implement services like Git servers that need to know which user authenticated (not just the SSH username).
