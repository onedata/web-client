%% Copyright (C) 2012-2013 Jeremy Ong
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
%% OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
%% ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% https://github.com/jeremyong/websocket_client/
%%
%% ===================================================================
%% @doc: This is a simple echo server written in cowboy
%% used for WS client tests.
%% @end
%% ===================================================================
-module(echo_server).

-behaviour(cowboy_websocket_handler).

-export([
    start/0
]).

% Cowboy callbacks
-export([
    init/3,
    websocket_init/3,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

-record(state, {}).

% Start the cowboy server
start() ->
    io:format("Starting echo server.~n"),
    Dispatch = cowboy_router:compile([{'_', [
        {"/hello/", ?MODULE, []},
        {'_', ?MODULE, []}
    ]}]),
    {ok, _} = cowboy:start_http(echo_listener, 2, [
        {port, 8080},
        {max_connections, 100}
    ],
        [{env, [{dispatch, Dispatch}]}]),
    ok.


% Upgrade to websocket in init
init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

% Called after websocket connection is established
websocket_init(_Transport, Req, _Opts) ->
    case cowboy_req:qs_val(<<"q">>, Req) of
        {undefined, Req2} -> {ok, Req2, #state{}};
        {Text, Req2} -> self() ! {send, Text},
            {ok, Req2, #state{}}
    end.

% Called when a frame arrives, just send the same message back
websocket_handle(Frame, Req, State) ->
    {reply, Frame, Req, State}.

% Called when the controlling process receives a message
% This oneis used to schedule a send action in init
websocket_info({send, Text}, Req, State) ->
    {reply, {text, Text}, Req, State};

% Called when the controlling process receives a message
websocket_info(_Msg, Req, State) ->
    {ok, Req, State}.

% Called when the WS connection is closed
websocket_terminate(_Reason, _Req, _State) ->
    ok.
