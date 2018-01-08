%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Util functions for setting ssl options based on security requirements.
%%% @end
%%%--------------------------------------------------------------------
-module(secure_ssl_opts).
-author("Lukasz Opiola").

-include_lib("public_key/include/OTP-PUB-KEY.hrl").

%% API
-export([expand/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Expands ssl options for peer verification based on desired level of security:
%%      {secure, true} will verify peercert and hostname
%%      {secure, only_verify_peercert} will only verify peercert
%%      {secure, false} will NOT PERFORM ANY validation.
%% The force_insecure_connections env variable can be used to override whatever
%% is passed as secure flag and skip any verification.
%% @end
%%--------------------------------------------------------------------
-spec expand(http_client:url(), http_client:ssl_opts()) -> ssl:ssl_opts().
expand(Url, SslOpts) ->
    ForceInsecure = application:get_env(web_client, force_insecure_connections, false),
    SecureFlag = proplists:get_value(secure, SslOpts, true),
    NewOpts = case ForceInsecure orelse SecureFlag =:= false of
        true ->
            [{verify, verify_none}];
        false ->
            CustomCaCerts = proplists:get_value(cacerts, SslOpts, []),
            CaCerts = CustomCaCerts ++ certifi:cacerts(),
            CommonOpts = [
                {verify, verify_peer},
                {depth, 99},
                {cacerts, CaCerts},
                {partial_chain, fun(Certs) -> partial_chain(CaCerts, Certs) end}
            ],
            case SecureFlag of
                true ->
                    #{host := Host} = url_utils:parse(Url),
                    VerifyFun = {
                        fun ssl_verify_hostname:verify_fun/3,
                        [{check_hostname, binary_to_list(Host)}]
                    },
                    [{verify_fun, VerifyFun} | CommonOpts];
                only_verify_peercert ->
                    CommonOpts
            end
    end,
    NewOpts ++ proplists:delete(cacerts, proplists:delete(secure, SslOpts)).

%%%===================================================================
%%% Private functions; code from hackney (MIT licence) and rebar3 (BSD license)
%%%===================================================================

-spec partial_chain(CaCertDers :: [binary()], CertDers :: [binary()]) ->
    unknown_ca | {trusted_ca, 'OTPCertificate'}.
partial_chain(CaCertDers, CertDers) ->
    Certs = lists:reverse([{Cert, public_key:pkix_decode_cert(Cert, otp)} ||
        Cert <- CertDers]),
    CaCerts = [public_key:pkix_decode_cert(Cert, otp) || Cert <- CaCertDers],

    case find(fun({_, Cert}) -> check_cert(CaCerts, Cert) end, Certs) of
        {ok, Trusted} ->
            {trusted_ca, element(1, Trusted)};
        _ ->
            unknown_ca
    end.


-spec check_cert(CaCerts :: [#'OTPCertificate'{}], Cert :: [#'OTPCertificate'{}]) ->
    boolean().
check_cert(CaCerts, Cert) ->
    lists:any(fun(CaCert) ->
        extract_public_key_info(CaCert) == extract_public_key_info(Cert)
    end, CaCerts).


-spec extract_public_key_info(#'OTPCertificate'{}) -> #'SubjectPublicKeyInfo'{}.
extract_public_key_info(Cert) ->
    Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo.


-spec find(fun(), list()) -> {ok, term()} | error.
find(Fun, [Head | Tail]) when is_function(Fun) ->
    case Fun(Head) of
        true ->
            {ok, Head};
        false ->
            find(Fun, Tail)
    end;
find(_Fun, []) ->
    error.