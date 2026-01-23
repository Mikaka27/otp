%%% Simple Echo Server - Complete Working Example
%%% Demonstrates ssh_server_channel behavior
-module(simple_echo_server).
-behaviour(ssh_server_channel).

%% API
-export([start_daemon/1, stop_daemon/1]).

%% ssh_server_channel callbacks
-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-record(state, {
    cm,           % Connection manager reference
    channel_id    % Channel ID
}).

%%%===================================================================
%%% API
%%%===================================================================

%% Start SSH daemon with echo subsystem on given port
start_daemon(Port) ->
    ssh:start(),
    ssh:daemon(Port, [
        {subsystems, [{"echo", {?MODULE, []}}]},
        {system_dir, "."},  % Host keys directory
        {user_passwords, [{"test", "test"}]},
        {pwdfun, fun(_User, _Pass) -> true end}  % Accept any password for demo
    ]).

stop_daemon(DaemonRef) ->
    ssh:stop_daemon(DaemonRef).

%%%===================================================================
%%% ssh_server_channel callbacks
%%%===================================================================

%% Initialize the channel
init([]) ->
    io:format("[Echo Server] Channel initializing~n"),
    {ok, #state{}}.

%% Handle the ssh_channel_up message (first message received)
handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State) ->
    io:format("[Echo Server] Channel ~p up on connection ~p~n", 
              [ChannelId, ConnectionManager]),
    %% Send welcome message
    Msg = "Welcome to Echo Server! Type something...\r\n",
    ssh_connection:send(ConnectionManager, ChannelId, Msg),
    {ok, State#state{cm = ConnectionManager, channel_id = ChannelId}};

handle_msg(Msg, State) ->
    io:format("[Echo Server] Unexpected msg: ~p~n", [Msg]),
    {ok, State}.

%% Handle SSH Connection Protocol messages
handle_ssh_msg({ssh_cm, CM, {data, Ch, 0, Data}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    io:format("[Echo Server] Received data: ~p~n", [Data]),
    %% Echo the data back
    ssh_connection:send(CM, Ch, ["Echo: ", Data]),
    {ok, State};

handle_ssh_msg({ssh_cm, CM, {eof, Ch}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    io:format("[Echo Server] EOF received~n"),
    {ok, State};

handle_ssh_msg({ssh_cm, CM, {signal, Ch, Signal}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    io:format("[Echo Server] Signal received: ~p~n", [Signal]),
    {ok, State};

handle_ssh_msg({ssh_cm, CM, {exit_signal, Ch, Signal, Error, _}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    io:format("[Echo Server] Exit signal: ~p, Error: ~p~n", [Signal, Error]),
    {stop, Ch, State};

handle_ssh_msg({ssh_cm, CM, {exit_status, Ch, Status}}, 
               #state{cm = CM, channel_id = Ch} = State) ->
    io:format("[Echo Server] Exit status: ~p~n", [Status]),
    {stop, Ch, State};

handle_ssh_msg(Msg, State) ->
    io:format("[Echo Server] Unexpected SSH msg: ~p~n", [Msg]),
    {ok, State}.

terminate(Reason, _State) ->
    io:format("[Echo Server] Terminating: ~p~n", [Reason]),
    ok.

%%%===================================================================
%%% Usage:
%%%
%%% 1. Start the server:
%%%    {ok, Daemon} = simple_echo_server:start_daemon(2222).
%%%
%%% 2. Connect from another terminal:
%%%    ssh -p 2222 test@localhost -s echo
%%%
%%% 3. Type messages and see them echoed back
%%%
%%% 4. Stop the server:
%%%    simple_echo_server:stop_daemon(Daemon).
%%%===================================================================
