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

%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2015 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module is taken from 'websocket_client' project
%% and slightly adopted for onedata needs.
%%
%% @end
%% ===================================================================
-module(websocket_client_handler_behaviour).

-type state() :: any().
-type keepalive() :: integer().
-type close_type() :: normal | error | remote.

-callback init(list(), websocket_req:req()) ->
    {ok, state()} |
    {ok, state(), keepalive()}.

-callback websocket_handle(
    {text | binary | ping | pong, binary()},
    websocket_req:req(),
    state()) ->
    {ok, state()} |
    {reply, websocket_req:frame(), state()} |
    {close, binary(), state()}.

-callback websocket_info(any(), websocket_req:req(), state()) ->
    {ok, state()}
    | {reply, websocket_req:frame(), state()}
    | {close, binary(), state()}.

-callback websocket_terminate(
    {close_type(), term()} | {close_type(), integer(), binary()},
    websocket_req:req(),
    state()) -> ok.
