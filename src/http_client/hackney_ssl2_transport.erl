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

%%--------------------------------------------------------------------
%% @doc
%% Atoms used to identify messages in {active, once | true} mode.
%% Returns atom recognizable by hackney.
%% @end
%%--------------------------------------------------------------------
-spec messages(term()) -> {ssl, ssl_closed, ssl_error}.
messages(_) ->
    {ssl, ssl_closed, ssl_error}.


%%--------------------------------------------------------------------
%% @doc
%% @equiv connect(Host, Port, Opts, infinity)
%% @end
%%--------------------------------------------------------------------
-spec connect(Host :: list(), Port :: inet:port_number(),
    Opts :: [http_client:opt()]) ->
    {ok, Socket :: ssl2:socket()} | {error, Reason :: atom()}.
connect(Host, Port, Opts) ->
    connect(Host, Port, Opts, infinity).


%%--------------------------------------------------------------------
%% @doc
%% Opens an ssl connection to Host, Port.
%% @see ssl2:connect/4
%% @end
%%--------------------------------------------------------------------
-spec connect(Host :: list(), Port :: inet:port_number(),
    Opts :: [http_client:opt()], Timeout :: timeout()) ->
    {ok, Socket :: ssl2:socket()} | {error, Reason :: atom()}.
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
    ConnectOpts = hackney_util:filter_options(Opts, AcceptedOpts, BaseOpts),
    ssl2:connect(Host, Port, ConnectOpts, Timeout).


%%--------------------------------------------------------------------
%% @doc
%% @equiv recv(Socket, Length, infinity)
%% @end
%%--------------------------------------------------------------------
-spec recv(Socket :: ssl2:socket(), Size :: non_neg_integer()) ->
    {ok, binary()} | {error, Reason :: closed | timeout | atom()}.
recv(Socket, Length) ->
    recv(Socket, Length, infinity).


%%--------------------------------------------------------------------
%% @doc
%% Receives a packet from a socket in passive mode.
%% If the socket is closed, returns {error, closed}.
%% @see ssl2:recv/3
%% @end
%%--------------------------------------------------------------------
-spec recv(Socket :: ssl2:socket(), Size :: non_neg_integer(),
    Timeout :: timeout()) ->
    {ok, binary()} | {error, Reason :: closed | timeout | atom()}.
recv(Socket, Length, Timeout) ->
    ssl2:recv(Socket, Length, Timeout).


%%--------------------------------------------------------------------
%% @doc
%% Writes Data to Socket.
%% If the socket is closed, returns {error, closed}.
%% @see ssl2:send/2
%% @end
%%--------------------------------------------------------------------
-spec send(ssl2:socket(), iolist()) -> ok | {error, atom()}.
send(Socket, Data) ->
    ssl2:send(Socket, Data).


%%--------------------------------------------------------------------
%% @doc
%% Sets options according to Options for the socket Socket.
%% @see ssl2:setopts/2
%% @end
%%--------------------------------------------------------------------
-spec setopts(Socket :: ssl2:socket(), Opts :: [http_client:opt()]) -> ok.
setopts(Socket, Opts) ->
    ssl2:setopts(Socket, Opts).


%%--------------------------------------------------------------------
%% @doc
%% Assigns a new controlling process to the socket.
%% A controlling process receives all messages from the socket.
%% @see ssl2:controlling_process/2
%% @end
%%--------------------------------------------------------------------
-spec controlling_process(Socket :: ssl2:socket(),
    NewControllingProcess :: pid()) -> ok.
controlling_process(Socket, Pid) ->
    ssl2:controlling_process(Socket, Pid).


%%--------------------------------------------------------------------
%% @doc
%% Returns the address and port number of the peer.
%% @see ssl2:peername/1
%% @end
%%--------------------------------------------------------------------
-spec peername(Socket :: ssl2:socket()) ->
    {ok, {inet:ip_address(), inet:port_number()}} | {error, Reason :: atom()}.
peername(Socket) ->
    ssl2:peername(Socket).


%%--------------------------------------------------------------------
%% @doc
%% Gracefully closes the socket.
%% @see ssl2:close/1
%% @end
%%--------------------------------------------------------------------
-spec close(Socket :: ssl2:socket()) -> ok | {error, Reason :: atom()}.
close(Socket) ->
    ssl2:close(Socket).


%%--------------------------------------------------------------------
%% @doc
%% Shuts down the connection in one or two directions.
%% To be able to handle that the peer has done a shutdown on the write
%% side, the {exit_on_close, false} option is useful.
%% @see ssl2:shutdown/2
%% @end
%%--------------------------------------------------------------------
-spec shutdown(Socket :: ssl2:socket(), Type :: read | write | read_write) ->
    ok | {error, Reason :: atom()}.
shutdown(Socket, How) ->
    ssl2:shutdown(Socket, How).

%%--------------------------------------------------------------------
%% @doc
%% Returns the address and port number of the socket.
%% @see ssl2:sockname/1
%% @end
%%--------------------------------------------------------------------
-spec sockname(SocketOrAcceptor :: ssl2:socket() | ssl2:acceptor()) ->
    {ok, {inet:ip_address(), inet:port_number()}} | {error, Reason :: atom()}.
sockname(SocketOrAcceptor) ->
    ssl2:sockname(SocketOrAcceptor).
