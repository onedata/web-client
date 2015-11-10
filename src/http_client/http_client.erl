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
%% Options are passed directly to hackney. For possible options, see:
%% https://github.com/benoitc/hackney/
%%
%% To perform a https request without ceryfying the server cert, use
%% 'insecure' option.
%%
%% To pass ssl options to hackney, use the option {ssl_options, [<Options>]}.
%%
%% @end
%% ===================================================================
-module(http_client).
-author("lopiola").

-include("logging.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

% Allowes m ethods - standard HTTP/1.1 and some more
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
% Request options
-type opts() :: [tuple()].
% Response code
-type code() :: integer().


-export_type([method/0, url/0, headers/0, body/0, opts/0, code/0]).

%% API - convenience functions
-export([get/1, get/2, get/3, get/4]).
-export([post/1, post/2, post/3, post/4]).
-export([put/1, put/2, put/3, put/4]).
-export([delete/1, delete/2, delete/3, delete/4]).
-export([request/1, request/2, request/3, request/4]).
% Performs the request
-export([request/5]).
% Other
-export([ca_bundle_location/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Informs where CA bundle is expected to be found. It is required
%% for server cert verification
%% @end
%%--------------------------------------------------------------------
ca_bundle_location() ->
    "/etc/ssl/cert.pem".


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
    HcknURL0 = hackney_url:parse_url(URL),
    {HcknURL, PreparedOpts} =
        % In case of HTTPS request, use our SSL rather than erlang's
    case HcknURL0#hackney_url.transport of
        hackney_ssl_transport ->
            HURL = HcknURL0#hackney_url{transport = hackney_ssl2_transport},
            % Custom ssl opts
            {HURL, prepare_ssl_opts(Options)};
        _ ->
            {HcknURL0, Options}
    end,

    case do_request(Method, HcknURL, ReqHdrs, ReqBd, PreparedOpts) of
        {ok, Code, RespHeaders, RespBody} ->
            {ok, Code, RespHeaders, RespBody};
        {error, closed} ->
            % Hackney uses socket pools, sometimes it grabs a
            % disconnected socket and returns {error, closed}.
            % Try again (once) if this happens.
            % @todo check why and when this happens
            % @todo maybe it is connected with using custom transport
            % @todo   and hackney calls some callback from default one
            % @todo maybe its ssl2 problem
            ?debug("Hackney request returned {error, closed}, retrying."),
            do_request(Method, HcknURL, ReqHdrs, ReqBd, PreparedOpts);
        {error, Error} ->
            {error, Error}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%--------------------------------------------------------------------
%% @doc
%% Calls hackney to perform a HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec do_request(Method :: method(), HcknURL :: hackney_url(),
    ReqHdrs :: headers(), ReqBd :: body(), Options :: opts()) ->
    {ok, code(), headers(), body()} | {error, term()}.
do_request(Method, HcknURL, ReqHdrs, ReqBd, Options) ->
    case hackney:request(Method, HcknURL, ReqHdrs, ReqBd, Options) of
        {ok, Code, RespHeaders} ->
            {ok, Code, RespHeaders, <<>>};
        {ok, Code, RespHeaders, Ref} ->
            {ok, RespBody} = hackney:body(Ref),
            {ok, Code, RespHeaders, RespBody};
        {error, Error} ->
            {error, Error}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Prepares options for hackney. Analyses ssl_options passed in Options list
%% and merges them with hackney's default options. This is needed because
%% hackney won't perform merge - if it finds ssl_options specified by
%% user, it overrides its default options and cert
%% verification is not performed.
%% @end
%%--------------------------------------------------------------------
-spec prepare_ssl_opts(Options :: opts()) -> Options :: opts().
prepare_ssl_opts(Options) ->
    SSLOpts = proplists:get_value(ssl_options, Options, []),
    case proplists:get_value(insecure, Options, undefined) of
        true ->
            % Insecure option is present,
            % don't add verify flag (don't modify SSL opts at all)
            [{connect_options, SSLOpts} | proplists:delete(ssl_options, Options)];
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
            [{connect_options, SSLOMerged} | proplists:delete(ssl_options, Options)]
    end.
