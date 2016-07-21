# web-client
*web-client* is an erlang library including HTTP and WebSocket client.

The HTTP client is a wrapper of [hackney](https://github.com/benoitc/hackney) library. The difference is that [etls](https://github.com/onedata/erlang-tls) is used for underlying transport for HTTPS requests and for server certificate verification againts system's CA bundle.

The WebSocket client is a modification of [websocket_client](https://github.com/jeremyong/websocket_client).

# User Guide
Add *web-client* as a`rebar` dependency:

```erlang
{deps, [
    {web_client, "1.0.1", {git, "https://github.com/onedata/web-client.git", {tag, "1.0.1"}}}
]}.
```

Do not forget to add *web_client* to dependencies in your app.src file so that it is started before your application.

## Using HTTP client:

```erlang
% Simplest GET
http_client:request(get, <<"domain.com">>).

% With headers
http_client:request(get, <<"domain.com">>, [{<<"content-type">>, <<"application/json">>}]).

% With headers and body
http_client:request(get, <<"domain.com">>, [{<<"content-type">>, <<"application/json">>}], <<"Request body">>).

% With headers, body and options (see http_client.erl for all possible options)
http_client:request(get, <<"google.com">>, [{<<"content-type">>, <<"application/json">>}], <<"Request body">>, [insecure]).

% Convenience functions
http_client:get(<<"domain.com">>).
http_client:post(<<"domain.com">>).
http_client:put(<<"domain.com">>).
http_client:delete(<<"domain.com">>).
```

## Using WebSocket client:

```erlang
% Start WebSocket connection 
{ok, Pid} = websocket_client:start_link("ws://domain.com", my_ws_handler, [], []).

% Start secure WebSocket connection 
{ok, Pid} = websocket_client:start_link("wss://domain.com", my_ws_handler, [], []).


% =====================
% my_ws_handler.erl

% Module that handles incoming messages
-module(my_ws_handler).
-behaviour(websocket_client_handler_behaviour).

init([], _ConnState) ->
    {ok, undefined}.
	
websocket_handle(Msg, _ConnState, State) ->
    io:format("Message: ~p~n", [Msg]),
    {ok, State}.
	
websocket_info(Info, _ConnState, State) ->
    io:format("Info: ~p~n", [Info]),
    {ok, State}.

websocket_terminate(_Reason, _ConnState, _State) ->
    ok.
```


# API

HTTP client API (```http_client.erl```):

* ```get/1, get/2, get/3, get/4```
* ```post/1, post/2, post/3, post/4```
* ```put/1, put/2, put/3, put/4```
* ```delete/1, delete/2, delete/3, delete/4```
* ```request/1, request/2, request/3, request/4, request/5```
* ```request_return_stream/5```

The functions with different arities can be used when you don't want to pass headers, body or options, just use default (or empty) value.

WebSocket client API (```websocket_client.erl```):

* ```start_link/4```
* ```cast/2```
* ```send/2```
* ```ws_client_init/7```


WebSocket client handler behaviour API (```websocket_client_handler_behaviour.erl```):

* ```init/2```
* ```websocket_handle/3```
* ```websocket_info/3```
* ```websocket_terminate/3```



# web-client in Onedata
*web_client* is used in onedata in all projects for unified HTTP and WebSocket client API.

