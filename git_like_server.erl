%%% Git-Like Server - Shows the Authentication Problem
%%% Demonstrates why the requested feature is needed
-module(git_like_server).
-behaviour(ssh_server_channel).

%% API
-export([start_daemon/1]).

%% ssh_server_channel callbacks
-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-record(state, {
    cm,
    channel_id,
    command,
    ssh_user      % This is just "git", not the real user!
}).

%%%===================================================================
%%% API
%%%===================================================================

start_daemon(Port) ->
    ssh:start(),
    ssh:daemon(Port, [
        %% Everyone connects as "git"
        {user_passwords, [{"git", "git"}]},
        
        %% Custom key authentication
        {key_cb, {git_key_handler, []}},
        
        %% Use this module as exec handler
        {exec, {?MODULE, []}},
        
        {system_dir, "."}
    ]).

%%%===================================================================
%%% ssh_server_channel callbacks
%%%===================================================================

%% Init receives the command as the last argument
init([Command]) ->
    io:format("[Git Server] Init with command: ~p~n", [Command]),
    {ok, #state{command = Command}}.

handle_msg({ssh_channel_up, ChannelId, CM}, State) ->
    io:format("[Git Server] Channel up: ~p~n", [ChannelId]),
    
    %% ============================================================
    %% IMPORTANT: For exec handlers, ONE channel = ONE command!
    %% ============================================================
    %% 
    %% Git workflow:
    %% 1. Client runs: git clone ssh://git@server/repo.git
    %% 2. SSH connects, authenticates
    %% 3. Opens ONE channel
    %% 4. Executes ONE command: "git-upload-pack '/repo.git'"
    %% 5. Git protocol data flows over this channel
    %% 6. Channel closes when command completes
    %% 7. For next operation (e.g., git push), NEW connection + NEW channel
    %%
    %% So we execute the command immediately when channel comes up.
    %%
    %% ============================================================
    %% THE PROBLEM: We need to know which user authenticated!
    %% ============================================================
    %% 
    %% But we only know:
    %% - SSH username is "git" (everyone uses this)
    %% - The command being executed
    %% 
    %% We DON'T know:
    %% - Which public key was used to authenticate
    %% - Which real user owns that key
    %% - What permissions that user has
    %%
    
    case parse_git_command(State#state.command) of
        {ok, Operation, RepoPath} ->
            io:format("[Git Server] Operation: ~p, Repo: ~p~n", 
                      [Operation, RepoPath]),
            io:format("[Git Server] *** PROBLEM: Don't know which user! ***~n"),
            
            %% In a real implementation:
            %% RealUser = get_authenticated_user(),  % NOT POSSIBLE!
            %% case check_permission(RealUser, Operation, RepoPath) of
            %%     true -> 
            %%         spawn_git_process(CM, ChannelId, Operation, RepoPath),
            %%         {ok, State#state{cm = CM, channel_id = ChannelId}};
            %%     false -> 
            %%         send_error(CM, ChannelId, "Permission denied"),
            %%         {stop, ChannelId, State}
            %% end
            
            send_error(CM, ChannelId, "Cannot determine user identity"),
            {stop, ChannelId, State#state{cm = CM, channel_id = ChannelId}};
        
        {error, Reason} ->
            send_error(CM, ChannelId, Reason),
            {stop, ChannelId, State#state{cm = CM, channel_id = ChannelId}}
    end.

handle_ssh_msg({ssh_cm, CM, {data, Ch, 0, Data}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    %% In a real implementation, this would be Git protocol data
    %% from the git-upload-pack or git-receive-pack process
    io:format("[Git Server] Received data: ~p bytes~n", [byte_size(Data)]),
    {ok, State};

handle_ssh_msg({ssh_cm, CM, {eof, Ch}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    io:format("[Git Server] EOF received, command completed~n"),
    {ok, State};

handle_ssh_msg({ssh_cm, CM, {exit_status, Ch, Status}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    io:format("[Git Server] Command exited with status: ~p~n", [Status]),
    {stop, Ch, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_git_command(Command) ->
    case string:tokens(Command, " ") of
        ["git-upload-pack", RepoPath] ->
            {ok, read, string:trim(RepoPath, both, "'")};
        ["git-receive-pack", RepoPath] ->
            {ok, write, string:trim(RepoPath, both, "'")};
        _ ->
            {error, "Unknown git command"}
    end.

send_error(CM, Ch, Message) ->
    ssh_connection:send(CM, Ch, ["Error: ", Message, "\n"]),
    ssh_connection:send_eof(CM, Ch),
    ssh_connection:exit_status(CM, Ch, 1).

%%%===================================================================
%%% Custom Key Handler (shows where we'd LIKE to pass state)
%%%===================================================================

-module(git_key_handler).
-behaviour(ssh_server_key_api).

-export([host_key/2, is_auth_key/3]).

host_key(Algorithm, DaemonOptions) ->
    ssh_file:host_key(Algorithm, DaemonOptions).

%% This is where we identify the real user by their public key
is_auth_key(Key, "git", _Opts) ->
    %% Look up which user owns this key
    RealUser = case lookup_user_by_key(Key) of
        {ok, User} -> User;
        error -> undefined
    end,
    
    io:format("[Key Handler] Key belongs to user: ~p~n", [RealUser]),
    
    %% ============================================================
    %% THE PROBLEM: We can only return true/false!
    %% ============================================================
    %% 
    %% We WANT to return: {true, #{real_user => RealUser}}
    %% So the channel handler can access this information
    %% 
    %% But we can ONLY return: true | false
    %%
    %% We could try storing in ETS, but:
    %% - What key to use? Connection PID? Channel PID? (they're different!)
    %% - Race conditions between auth and channel start
    %% - Cleanup on connection close is unreliable
    %%
    
    %% For now, just accept the key
    RealUser =/= undefined.

lookup_user_by_key(_Key) ->
    %% In real implementation, look up in database
    %% For demo, just return a fake user
    {ok, "alice"}.

%%%===================================================================
%%% Usage Example:
%%%
%%% 1. Start server:
%%%    {ok, Daemon} = git_like_server:start_daemon(2222).
%%%
%%% 2. Try to clone (will fail with our error):
%%%    git clone ssh://git@localhost:2222/user/repo.git
%%%
%%% 3. The server will show:
%%%    [Key Handler] Key belongs to user: "alice"
%%%    [Git Server] *** PROBLEM: Don't know which user! ***
%%%
%%% This demonstrates why the requested feature (passing state from
%%% is_auth_key to channel init) is necessary for Git servers.
%%%===================================================================
