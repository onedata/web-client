%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2015 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains useful functions concerning
%% both http and websocket clients.
%% @end
%% ===================================================================
-module(web_client_utils).

%% API
-export([ca_bundle_location/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Informs where CA bundle is expected to be found. It is required
%% for server cert verification.
%% @end
%%--------------------------------------------------------------------
-spec ca_bundle_location() -> string().
ca_bundle_location() ->
    "/etc/ssl/cert.pem".
