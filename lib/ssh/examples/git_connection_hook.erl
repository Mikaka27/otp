%%% Example connection hook for Git server
-module(git_connection_hook).
-behaviour(ssh_connection_hook).

-export([init/2, auth_completed/4, channel_request/5, terminate/2]).

init(_ConnectionRef, _Opts) ->
    {ok, #{user => undefined, repos => [], exec_count => 0}}.

auth_completed(User, "publickey", _AuthData, State) ->
    %% Look up which repositories this user can access
    Repos = lookup_user_repos(User),
    {ok, State#{user => User, repos => Repos}}.

channel_request(_ChannelId, "exec", Cmd, _WantReply, State) ->
    %% Parse git command
    case parse_git_command(Cmd) of
        {ok, Repo, Action} ->
            %% Check if user has access to this repo
            case is_authorized(State, Repo, Action) of
                true ->
                    NewState = State#{exec_count => maps:get(exec_count, State) + 1},
                    {allow, NewState};
                false ->
                    {deny, State}
            end;
        {error, _} ->
            {deny, State}
    end;
channel_request(_ChannelId, _Type, _Data, _WantReply, State) ->
    {allow, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal functions
lookup_user_repos(User) ->
    %% In real implementation, query database
    case User of
        "alice" -> ["repo1", "repo2"];
        "bob" -> ["repo2"];
        _ -> []
    end.

parse_git_command(Cmd) when is_binary(Cmd) ->
    parse_git_command(binary_to_list(Cmd));
parse_git_command(Cmd) ->
    case string:tokens(Cmd, " ") of
        ["git-upload-pack", Repo] -> {ok, clean_repo(Repo), read};
        ["git-receive-pack", Repo] -> {ok, clean_repo(Repo), write};
        _ -> {error, invalid_command}
    end.

clean_repo(Repo) ->
    %% Remove quotes and .git suffix
    R1 = string:trim(Repo, both, "'\""),
    string:trim(R1, trailing, ".git").

is_authorized(#{repos := Repos}, Repo, _Action) ->
    lists:member(Repo, Repos).
