%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2015 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module tests the http_client module, which is a wrapper
%% on hackney. The tests check if API calls are correctly translated
%% to hackney calls.
%% @end
%% ===================================================================
-module(http_client_tests).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
    {setup,
        % Setup
        fun() ->
            meck:new(hackney)
        end,
        % Teardown
        fun(_) ->
            meck:unload(hackney)
        end,
        [
            {"API test", fun api_t/0}
        ]
    }.


% Checks if function calls are properly translated to hackney calls
api_t() ->
    % Just return te args, they will be checked if OK.
    meck:expect(hackney, request,
        fun(Mthd, URL, Hdrs, Bd, Opts) ->
            {Mthd, URL, Hdrs, Bd, Opts}
        end),

    % Some test values
    HTTP_URL = <<"http://test.url">>,
    HTTPS_URL = <<"https://test.url">>,
    Headers = [{<<"key">>, <<"value">>}],
    Body = <<"body">>,

    % Specify some tests in tuples(3)
    % {
    %   method_to_call,
    %   args,
    %   expected args in call to hackney:request
    % }

    % 1. test: HTTP request
    Test1 = {
        get,
        [HTTP_URL, Headers, Body],
        [get, HTTP_URL, Headers, Body, []]
    },

    % 2. test: HTTP request
    Test2 = {
        head,
        [HTTP_URL],
        [head, HTTP_URL, [], <<>>, []]
    },

    % 3. test: HTTP request
    Test3 = {
        request,
        [get, HTTP_URL, Headers],
        [get, HTTP_URL, Headers, <<>>, []]
    },

    % 4. test: HTTPS request
    Test4 = {
        post,
        [HTTPS_URL, Headers],
        [post, HTTPS_URL, Headers, <<>>,
            [{connect_options, [{verify_type, verify_peer}]}]]
    },

    % 5. test: HTTPS request with some ssl options
    Test5 = {
        post,
        [HTTPS_URL, Headers, Body,
            [option, {ssl_options, [{keyfile, "a"}, {certfile, "b"}]}]],
        [post, HTTPS_URL, Headers, <<>>,
            [
                {connect_options, [
                    {verify_type, verify_peer},
                    {keyfile, "a"}, {certfile, "b"}
                ]},
                option
            ]
        ]
    },

    % 6. test: HTTPS request with some ssl options overriding verify_type
    Test6 = {
        put,
        [HTTPS_URL, Headers, Body,
            [
                {ssl_options, [
                    {verify_type, verify_none},
                    {keyfile, "a"},
                    {certfile, "b"}
                ]}
            ]
        ],
        [put, HTTPS_URL, Headers, <<>>,
            [
                {connect_options, [
                    {verify_type, verify_none},
                    {keyfile, "a"},
                    {certfile, "b"}
                ]}
            ]
        ]
    },

    % 7. test: insecure HTTPS request
    Test7 = {
        post,
        [HTTPS_URL, Headers, Body, [insecure]],
        [post, HTTPS_URL, Headers, <<>>, [{connect_options, []}]]
    },

    % 8. test: insecure HTTPS request with some ssl options
    Test8 = {
        get,
        [HTTPS_URL, Headers, Body, [insecure, {ssl_options, [{keyfile, "a"}, {certfile, "b"}]}]],
        [post, HTTPS_URL, Headers, <<>>,
            [{connect_options, [{keyfile, "a"}, {certfile, "b"}]}]]
    },

    % Do the tests
    lists:foreach(
        fun({Method, Args, Expected}) ->
            ?assertEqual(Expected, erlang:apply(http_client, Method, Args))
        end, [Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8]).
