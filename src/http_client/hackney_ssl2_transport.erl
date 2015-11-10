%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2015 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module is an implementation of hackney transport interface.
%% It uses erlang-tls (ssl2) for communication.
%% @end
%% ===================================================================
-module(hackney_ssl2_transport).

%% API - conforms to hackney transport API
-export([messages/1,
    connect/3, connect/4,
    send/2,
    recv/3, recv/2,
    setopts/2,
    controlling_process/2,
    peername/1,
    close/1,
    shutdown/2,
    sockname/1
]).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Atoms used to identify messages in {active, once | true} mode.
messages(_) ->
    {ssl2, ssl2_closed, ssl2_error}.

connect(Host, Port, Opts) ->
    connect(Host, Port, Opts, infinity).

connect(Host, Port, Opts, Timeout) when is_list(Host), is_integer(Port),
    (Timeout =:= infinity orelse is_integer(Timeout)) ->
    % Accepted opts as in ssl2.erl
    AcceptedOpts = [
        active,
        packet,
        verify_type,
        fail_if_no_peer_cert,
        verify_client_once,
        rfc2818_verification_hostname,
        cacerts,
        crls,
        certfile,
        keyfile,
        chain
    ],
    %% filter options
    BaseOpts = [{active, false}, {packet, raw}],
    Opts1 = hackney_util:filter_options(Opts, AcceptedOpts, BaseOpts),
    ssl2:connect(Host, Port, Opts1, Timeout).

recv(Socket, Length) ->
    recv(Socket, Length, infinity).

%% @doc Receive a packet from a socket in passive mode.`
%% @see ssl2:recv/3
-spec recv(ssl2:socket(), non_neg_integer(), timeout())
        -> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
    ssl2:recv(Socket, Length, Timeout).

%% @doc Send a packet on a socket.
%% @see ssl2:send/2
-spec send(ssl2:socket(), iolist()) -> ok | {error, atom()}.
send(Socket, Packet) ->
    ssl2:send(Socket, Packet).

%% @doc Set one or more options for a socket.
%% @see ssl2:setopts/2
-spec setopts(ssl2:socket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
    ssl2:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see ssl2:controlling_process/2
-spec controlling_process(ssl2:socket(), pid())
        -> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
    ssl2:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see ssl2:peername/1
-spec peername(ssl2:socket())
        -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
    ssl2:peername(Socket).

%% @doc Close a TCP socket.
%% @see ssl2:close/1
-spec close(ssl2:socket()) -> ok.
close(Socket) ->
    ssl2:close(Socket).

%% @doc Immediately close a socket in one or two directions.
%% @see ssl2:shutdown/2
-spec shutdown(ssl2:socket(), read | write | read_write) -> ok.
shutdown(Socket, How) ->
    ssl2:shutdown(Socket, How).

%% @doc Get the local address and port of a socket
%% @see ssl2:sockname/1
-spec sockname(ssl2:socket())
        -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
    ssl2:sockname(Socket).
