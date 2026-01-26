%%% Complete example showing connection hook usage
-module(connection_hook_example).
-export([start_git_server/0, start_multi_hook_server/0, query_connection/1]).

%% Start a Git server with connection hook
start_git_server() ->
    ssh:daemon(2222, [
        {system_dir, "/etc/ssh"},
        {connection_hooks, [{git_connection_hook, []}]},
        {subsystems, []},
        {exec, {direct, fun git_exec/4}}
    ]).

%% Exec handler that uses session context
git_exec(Cmd, User, _PeerAddr, _AuthContext) ->
    io:format("Executing: ~s for user ~s~n", [Cmd, User]),
    %% Git command execution logic here
    {ok, "Git command executed"}.

%% Start server with multiple hooks
start_multi_hook_server() ->
    ssh:daemon(2223, [
        {system_dir, "/etc/ssh"},
        {connection_hooks, [
            {audit_hook, []},      % Logs all operations
            {quota_hook, []},      % Enforces quotas
            {git_connection_hook, []} % Git-specific logic
        ]}
    ]).

%% Query connection state from a channel process
query_connection(ConnectionHandler) ->
    %% Get state from specific hook
    case ssh_connection_handler:get_hook_state(ConnectionHandler, git_connection_hook) of
        {ok, State} ->
            io:format("Git hook state: ~p~n", [State]),
            State;
        {error, not_found} ->
            io:format("Hook not found~n"),
            undefined
    end,
    
    %% Get all hook states
    AllStates = ssh_connection_handler:get_all_hook_states(ConnectionHandler),
    io:format("All hook states: ~p~n", [AllStates]),
    AllStates.

%%% Example audit hook
-module(audit_hook).
-behaviour(ssh_connection_hook).
-export([init/2, auth_completed/4, channel_request/5, terminate/2]).

init(_ConnRef, _Opts) ->
    {ok, #{events => []}}.

auth_completed(User, Method, _AuthData, State) ->
    Event = {auth, User, Method, erlang:timestamp()},
    {ok, State#{events => [Event | maps:get(events, State)]}}.

channel_request(ChId, Type, _Data, _WantReply, State) ->
    Event = {request, ChId, Type, erlang:timestamp()},
    {allow, State#{events => [Event | maps:get(events, State)]}}.

terminate(_Reason, State) ->
    %% Write audit log
    Events = maps:get(events, State, []),
    io:format("Audit log: ~p~n", [lists:reverse(Events)]),
    ok.

%%% Example quota hook
-module(quota_hook).
-behaviour(ssh_connection_hook).
-export([init/2, channel_data/5]).

init(_ConnRef, _Opts) ->
    {ok, #{bytes => 0, limit => 10485760}}. % 10MB limit

channel_data(_ChId, _Type, Data, State) ->
    Bytes = maps:get(bytes, State) + byte_size(Data),
    Limit = maps:get(limit, State),
    if Bytes > Limit ->
        {disconnect, "Quota exceeded"};
    true ->
        {ok, State#{bytes => Bytes}}
    end.
