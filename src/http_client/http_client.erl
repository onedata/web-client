%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2015 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This is a HTTP client library, using hackney to perform requests.
%% It is used to unify all HTTP calls across onedata.
%%
%% The API includes:
%%      - some convenience functions (get/post/put/delete)
%%      - the main, general function request/5 that automatically returns the
%%          response body. Response format is {ok, Code, Headers, Body}
%%      - request_return_stream/5 - function that allows streaming the
%%          response body. Response format is {ok, StreamRef}.
%%          See hackney docs for streaming instructions.
%%
%% Possible options: see opts/0 type
%%
%% NOTE: If request_return_stream/5 is used, only the follow_redirect is taken
%%      into consideration for the redirection. If a valid redirection happens,
%%      the following is returned:
%%          {see_other, To, Headers} - for status 303 POST requests
%%          {redirect, To, Headers} - otherwise
%%
%%
%% @end
%% ===================================================================
-module(http_client).

-include_lib("hackney/include/hackney_lib.hrl").

% Allowed methods - standard HTTP/1.1 and some more
-type method() :: delete | get | head | post | put | connect | options | trace |
copy | lock | mkcol | move | propfind | proppatch | search | unlock | %% WEBDAV
report | mkactivity | checkout | merge | %% SUBVERSION
msearch | notify | subscribe | unsubscribe | %% UPNP
patch | purge. %% RFC-5789
% Request URL
-type url() :: string() | binary().
% Request / response headers
-type headers() :: [{Key :: string() | binary(), Value :: string() | binary()}].
% Request / response body
-type body() :: string() | binary().
% Response code
-type code() :: integer().
% Request options
-type opts() :: [opt()].

% All possible request options
-type opt() ::
%% to perform a https request without verifying the server cert
insecure |
%% to pass ssl options to ssl2
{ssl_options, [term()]} |
%% specifying maximum body length that can be automatically returned
%% from request. In case of a large body, function request_return_stream/5
%% can be used to stream the body.
{max_body, integer()} |
%% the response messages will be sent to this PID
%% (valid with request_return_stream/5)
{stream_to, pid()} |
%% to set a cookie or a list of cookies.
{cookie, binary() | [binary()]} |
%% false by default, automatically follow redirections
{follow_redirect, boolean()} |
%% 5 by default, the maximum number of redirections for a request
{max_redirect, integer()} |
%% false by default, to force the redirection even on POST
{force_redirect, boolean()} |
%% timeout used when estabilishing a connection, in milliseconds. Default: 8000.
{connect_timeout, infinity | integer()} |
%% timeout used when receiving a connection. Default: 5000.
{recv_timeout, infinity | integer()} |
%% to connect via a proxy
{proxy, proxy_opt()}.

% Proxy options (one of them can be used)
-type proxy_opt() ::
%% URL to use for the proxy. Used for basic HTTP proxy
binary() |
%% Host and port to connect, for HTTP proxy
{Host :: binary(), Port :: binary} |
%% Host and Port to connect
{socks5, Host :: binary(), Port :: binary()} |
%% Host and Port to connect to
{connect, Host :: binary(), Port :: binary()}.

% Opts passed to hackney
-type hackney_opts() :: [term()].

% Maximum body length that will be returned from request
-define(MAX_BODY_LENGTH, 1048576). % 1MB

%% API - convenience functions
-export([get/1, get/2, get/3, get/4]).
-export([post/1, post/2, post/3, post/4]).
-export([put/1, put/2, put/3, put/4]).
-export([delete/1, delete/2, delete/3, delete/4]).
-export([request/1, request/2, request/3, request/4]).
% Performs the request
-export([request/5]).
% Performs the request, but instead the body return the ref for streaming.
-export([request_return_stream/5]).

-export_type([method/0, url/0, headers/0, body/0, code/0,
    opts/0, opt/0, proxy_opt/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP GET request.
%% @end
%%--------------------------------------------------------------------
-spec get(URL :: url()) ->
    {ok, code(), headers(), body()} | {error, term()}.
get(URL) ->
    request(get, URL, [], <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP GET request.
%% @end
%%--------------------------------------------------------------------
-spec get(URL :: url(), Headers :: headers()) ->
    {ok, code(), headers(), body()} | {error, term()}.
get(URL, Headers) ->
    request(get, URL, Headers, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP GET request.
%% @end
%%--------------------------------------------------------------------
-spec get(URL :: url(), Headers :: headers(), Body :: body()) ->
    {ok, code(), headers(), body()} | {error, term()}.
get(URL, Headers, Body) ->
    request(get, URL, Headers, Body, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP GET request.
%% @end
%%--------------------------------------------------------------------
-spec get(URL :: url(), Headers :: headers(), Body :: body(),
    Options :: opts()) ->
    {ok, code(), headers(), body()} | {error, term()}.
get(URL, Headers, Body, Options) ->
    request(get, URL, Headers, Body, Options).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP POST request.
%% @end
%%--------------------------------------------------------------------
-spec post(URL :: url()) ->
    {ok, code(), headers(), body()} | {error, term()}.
post(URL) ->
    request(post, URL, [], <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP POST request.
%% @end
%%--------------------------------------------------------------------
-spec post(URL :: url(), Headers :: headers()) ->
    {ok, code(), headers(), body()} | {error, term()}.
post(URL, Headers) ->
    request(post, URL, Headers, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP POST request.
%% @end
%%--------------------------------------------------------------------
-spec post(URL :: url(), Headers :: headers(), Body :: body()) ->
    {ok, code(), headers(), body()} | {error, term()}.
post(URL, Headers, Body) ->
    request(post, URL, Headers, Body, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP POST request.
%% @end
%%--------------------------------------------------------------------
-spec post(URL :: url(), Headers :: headers(), Body :: body(),
    Options :: opts()) ->
    {ok, code(), headers(), body()} | {error, term()}.
post(URL, Headers, Body, Options) ->
    request(post, URL, Headers, Body, Options).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP PUT request.
%% @end
%%--------------------------------------------------------------------
-spec put(URL :: url()) ->
    {ok, code(), headers(), body()} | {error, term()}.
put(URL) ->
    request(put, URL, [], <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP PUT request.
%% @end
%%--------------------------------------------------------------------
-spec put(URL :: url(), Headers :: headers()) ->
    {ok, code(), headers(), body()} | {error, term()}.
put(URL, Headers) ->
    request(put, URL, Headers, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP PUT request.
%% @end
%%--------------------------------------------------------------------
-spec put(URL :: url(), Headers :: headers(), Body :: body()) ->
    {ok, code(), headers(), body()} | {error, term()}.
put(URL, Headers, Body) ->
    request(put, URL, Headers, Body, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP PUT request.
%% @end
%%--------------------------------------------------------------------
-spec put(URL :: url(), Headers :: headers(), Body :: body(),
    Options :: opts()) ->
    {ok, code(), headers(), body()} | {error, term()}.
put(URL, Headers, Body, Options) ->
    request(put, URL, Headers, Body, Options).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP DELETE request.
%% @end
%%--------------------------------------------------------------------
-spec delete(URL :: url()) ->
    {ok, code(), headers(), body()} | {error, term()}.
delete(URL) ->
    request(delete, URL, [], <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP DELETE request.
%% @end
%%--------------------------------------------------------------------
-spec delete(URL :: url(), Headers :: headers()) ->
    {ok, code(), headers(), body()} | {error, term()}.
delete(URL, Headers) ->
    request(delete, URL, Headers, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP DELETE request.
%% @end
%%--------------------------------------------------------------------
-spec delete(URL :: url(), Headers :: headers(), Body :: body()) ->
    {ok, code(), headers(), body()} | {error, term()}.
delete(URL, Headers, Body) ->
    request(delete, URL, Headers, Body, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP DELETE request.
%% @end
%%--------------------------------------------------------------------
-spec delete(URL :: url(), Headers :: headers(), Body :: body(),
    Options :: opts()) ->
    {ok, code(), headers(), body()} | {error, term()}.
delete(URL, Headers, Body, Options) ->
    request(delete, URL, Headers, Body, Options).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec request(URL :: url()) ->
    {ok, code(), headers(), body()} | {error, term()}.
request(URL) ->
    request(get, URL, [], <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec request(Method :: method(), URL :: url()) ->
    {ok, code(), headers(), body()} | {error, term()}.
request(Method, URL) ->
    request(Method, URL, [], <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec request(Method :: method(), URL :: url(), Headers :: headers()) ->
    {ok, code(), headers(), body()} | {error, term()}.
request(Method, URL, Headers) ->
    request(Method, URL, Headers, <<>>, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec request(Method :: method(), URL :: url(), Headers :: headers(),
    Body :: body()) ->
    {ok, code(), headers(), body()} | {error, term()}.
request(Method, URL, Headers, Body) ->
    request(Method, URL, Headers, Body, []).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec request(Method :: method(), URL :: url(), ReqHdrs :: headers(),
    ReqBd :: body(), Options :: opts()) ->
    {ok, code(), headers(), body()} | {error, term()}.
request(Method, URL, ReqHdrs, ReqBd, Options) ->
    % If max_body is specified in opts, don't modify it, else use default
    MaxBd = proplists:get_value(max_body, Options, ?MAX_BODY_LENGTH),
    % with_body option forces hackney to always return the body
    Opts = [with_body, {max_body, MaxBd} | proplists:delete(max_body, Options)],
    do_request(Method, URL, ReqHdrs, ReqBd, Opts).


%%--------------------------------------------------------------------
%% @doc
%% Performs a HTTP request and returns a reference that can be used to
%% stream the response body.
%% @end
%%--------------------------------------------------------------------
-spec request_return_stream(Method :: method(), URL :: url(),
    ReqHdrs :: headers(), ReqBd :: body(), Options :: opts()) ->
    {ok, StrmRef :: term()} | {error, term()}.
request_return_stream(Method, URL, ReqHdrs, ReqBd, Options) ->
    Opts = [async | Options],
    do_request(Method, URL, ReqHdrs, ReqBd, Opts).


%% ====================================================================
%% Internal functions
%% ====================================================================

%--------------------------------------------------------------------
%% @doc
%% Calls hackney to perform a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec do_request(Method :: method(), URL :: url(),
    ReqHdrs :: headers(), ReqBd :: body(), Options :: hackney_opts()) ->
    {ok, code(), headers(), body()} | {ok, StrmRef :: term()} | {error, term()}.
do_request(Mthd, URL, ReqHdrs, ReqBd, Options) ->
    HcknURL0 = hackney_url:parse_url(URL),
    {HcknURL, PreparedOpts} =
        case HcknURL0#hackney_url.transport of
            hackney_ssl_transport ->
                % Use ssl2 for HTTPS connections
                {
                    HcknURL0#hackney_url{transport = hackney_ssl2_transport},
                    prepare_ssl_opts(Options)
                };
            _ ->
                {
                    HcknURL0,
                    Options
                }
        end,
    % Do the request and return the outcome
    case hackney:request(Mthd, HcknURL, ReqHdrs, ReqBd, PreparedOpts) of
        {error, closed} ->
            io:format("HTTP request returned {error, closed}, retrying.~n"),
            % Hackney uses socket pools, sometimes it grabs a
            % disconnected socket and returns {error, closed}.
            % Try again (once) if this happens.
            % @todo check why and when this happens
            % @todo maybe it is connected with using custom transport
            % @todo   and hackney calls some callback from default one
            % @todo maybe its ssl2 problem
            hackney:request(Mthd, HcknURL, ReqHdrs, ReqBd, PreparedOpts);
        Result ->
            Result
    end.


%%--------------------------------------------------------------------
%% @doc
%% Prepares options for hackney. Analyses ssl_options passed in Options list
%% and transfors them in connect_opts, which will be fed to ssl2:connect.
%% @end
%%--------------------------------------------------------------------
-spec prepare_ssl_opts(Options :: opts()) -> Options :: opts().
prepare_ssl_opts(Options) ->
    SSLOpts = proplists:get_value(ssl_options, Options, []),
    ConnectOpts =
        case proplists:get_value(insecure, Options, undefined) of
            true ->
                % Insecure option is present,
                % don't add verify flag (don't modify SSL opts at all)
                SSLOpts;
            undefined ->
                % Insecure option is not present,
                % add verify flag if it's not present yet
                SSLOMerged =
                    case proplists:get_value(verify_type, SSLOpts, undefined) of
                        undefined ->
                            % Verify flag is not present, add it
                            [{verify_type, verify_peer} | SSLOpts];
                        _ ->
                            % Verify flag is present, do not modify ssl opts
                            SSLOpts
                    end,
                SSLOMerged
        end,
    % Remove ssl_options from the proplist - no longer needed
    NoSSLOpts = proplists:delete(ssl_options, Options),
    % Remove insecure from the proplist - no longer needed
    NoInsecureFlag = proplists:delete(insecure, NoSSLOpts),
    [{connect_options, ConnectOpts} | NoInsecureFlag].
