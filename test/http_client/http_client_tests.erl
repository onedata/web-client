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
-include_lib("hackney/include/hackney_lib.hrl").

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
            {"API test", fun api_t/0},
            {"request_return_stream test", fun request_return_stream_t/0}
        ]
    }.


% Checks if API function calls are properly translated to hackney calls
api_t() ->
    % Some test values
    HTTP_URL = <<"http://test.url">>,
    HTTPS_URL = <<"https://test.url">>,
    Parsed_HTTP_URL = hackney_url:parse_url(HTTP_URL),
    % If the URL is https, change the transport as in http_client
    HTTPS_U = hackney_url:parse_url(HTTPS_URL),
    Parsed_HTTPS_URL = HTTPS_U#hackney_url{transport = hackney_ssl_transport},
    HeadersMap = #{<<"key">> => <<"value">>},
    HeadersProplist = [{<<"key">>, <<"value">>}],
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
        [HTTP_URL, HeadersMap, Body],
        [get, Parsed_HTTP_URL, HeadersProplist, Body, [
            with_body,
            {max_body, undefined}
        ]]
    },

    % 2. test: HTTP request
    Test2 = {
        delete,
        [HTTP_URL],
        [delete, Parsed_HTTP_URL, [], <<>>, [
            with_body,
            {max_body, undefined}
        ]]
    },

    % 3. test: HTTP request
    Test3 = {
        request,
        [get, HTTP_URL, HeadersMap, Body, [
            {max_body, 12123}
        ]],
        [get, Parsed_HTTP_URL, HeadersProplist, Body, [
            with_body,
            {max_body, 12123}
        ]]
    },

    % 4. test: HTTPS request
    Test4 = {
        post,
        [HTTPS_URL, HeadersMap],
        [post, Parsed_HTTPS_URL, HeadersProplist, <<>>, [
            with_body,
            {max_body, undefined}
        ]]
    },

    % 5. test: HTTPS request with some ssl options
    Test5 = {
        post,
        [HTTPS_URL, HeadersMap, Body, [
            option,
            {ssl_options, [
                {keyfile, "a"}, {certfile, "b"}
            ]}
        ]],
        [post, Parsed_HTTPS_URL, HeadersProplist, Body, [
            option,
            {ssl_options, [
                {keyfile, "a"}, {certfile, "b"}
            ]},
            with_body,
            {max_body, undefined}
        ]]
    },

    % 6. test: HTTPS request with some ssl options overriding verify_type
    Test6 = {
        put,
        [HTTPS_URL, HeadersMap, Body, [
            {ssl_options, [
                {verify, verify_none},
                {keyfile, "a"},
                {certfile, "b"}
            ]}
        ]],
        [put, Parsed_HTTPS_URL, HeadersProplist, Body, [
            {ssl_options, [
                {verify, verify_none},
                {keyfile, "a"},
                {certfile, "b"}
            ]},
            with_body,
            {max_body, undefined}
        ]]
    },

    % 7. test: insecure HTTPS request
    Test7 = {
        post,
        [HTTPS_URL, HeadersMap, Body, [insecure]],
        [post, Parsed_HTTPS_URL, HeadersProplist, Body, [
            insecure,
            with_body,
            {max_body, undefined}
        ]]
    },

    % 8. test: insecure HTTPS request with some ssl options
    Test8 = {
        get,
        [HTTPS_URL, HeadersMap, Body, [
            insecure,
            {ssl_options, [{keyfile, "a"}, {certfile, "b"}]},
            {max_body, 987665}
        ]],
        [get, Parsed_HTTPS_URL, HeadersProplist, Body, [
            {ssl_options, [
                {keyfile, "a"}, {certfile, "b"}
            ]},
            insecure,
            with_body,
            {max_body, 987665}
        ]]
    },

    % Do the tests
    lists:foreach(
        fun({Method, Args, Expected}) ->
            meck:expect(hackney, request,
                fun(AMthd, AURL, AHdrs, ABd, AOpts) ->
                    % If args are not as expected, the test will fail here
                    compare_args([AMthd, AURL, AHdrs, ABd, AOpts], Expected),
                    {ok, 200, [], <<>>}
                end),
            ?assertEqual({ok, 200, #{}, <<>>}, erlang:apply(http_client, Method, Args))
        end, [Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8]),
    ?assert(meck:validate(hackney)).


% Checks if API function calls are properly translated to hackney calls
request_return_stream_t() ->
    % Some test values
    HTTP_URL = <<"http://test.url">>,
    HTTPS_URL = <<"https://test.url">>,
    Parsed_HTTP_URL = hackney_url:parse_url(HTTP_URL),
    % If the URL is https, change the transport as in http_client
    HTTPS_U = hackney_url:parse_url(HTTPS_URL),
    Parsed_HTTPS_URL = HTTPS_U#hackney_url{transport = hackney_ssl_transport},
    HeadersMap = #{<<"key">> => <<"value">>},
    HeadersProplist = [{<<"key">>, <<"value">>}],
    Body = <<"body">>,

    % Specify some tests in tuples(2).
    % The function called in tests is request_return_stream/5
    % {
    %   args,
    %   expected args in call to hackney:request
    % }

    % 1. test: HTTP request
    Test1 = {
        [post, HTTP_URL, HeadersMap, Body, []],
        [post, Parsed_HTTP_URL, HeadersProplist, Body, [
            async
        ]]
    },

    % 2. test: HTTP request
    Test2 = {
        [delete, HTTP_URL, HeadersMap, Body, [option]],
        [delete, Parsed_HTTP_URL, HeadersProplist, Body, [async, option]]
    },

    % 3. test: HTTPS request
    Test3 = {
        [get, HTTPS_URL, HeadersMap, Body, [option]],
        [get, Parsed_HTTPS_URL, HeadersProplist, Body, [
            async,
            option
        ]]
    },

    % 4. test: HTTPS request with some ssl options
    Test4 = {
        [put, HTTPS_URL, HeadersMap, Body, [
            option,
            {ssl_options, [
                {keyfile, "a"}, {certfile, "b"}
            ]}
        ]],
        [put, Parsed_HTTPS_URL, HeadersProplist, Body, [
            option,
            {ssl_options, [
                {keyfile, "a"},
                {certfile, "b"}
            ]},
            async
        ]]
    },

    % 5. test: HTTPS request with some ssl options overriding verify
    Test5 = {
        [head, HTTPS_URL, HeadersMap, Body, [
            {ssl_options, [
                {verify, verify_none},
                {keyfile, "a"},
                {certfile, "b"}
            ]}
        ]],
        [head, Parsed_HTTPS_URL, HeadersProplist, Body, [
            {ssl_options, [
                {verify, verify_none},
                {keyfile, "a"},
                {certfile, "b"}
            ]},
            async
        ]]
    },

    % 6. test: insecure HTTPS request
    Test6 = {
        [post, HTTPS_URL, HeadersMap, Body, [insecure]],
        [post, Parsed_HTTPS_URL, HeadersProplist, Body, [
            insecure,
            async
        ]]
    },

    % 7. test: insecure HTTPS request with some ssl options
    Test7 = {
        [delete, HTTPS_URL, HeadersMap, Body, [
            insecure,
            {ssl_options, [{keyfile, "a"}, {certfile, "b"}]}
        ]],
        [delete, Parsed_HTTPS_URL, HeadersProplist, Body, [
            {ssl_options, [
                {keyfile, "a"}, {certfile, "b"}
            ]},
            insecure,
            async
        ]]
    },

    % Do the tests
    lists:foreach(
        fun({Args, Expected}) ->
            meck:expect(hackney, request,
                fun(AMthd, AURL, AHdrs, ABd, AOpts) ->
                    % If args are not as expected, the test will fail here
                    compare_args([AMthd, AURL, AHdrs, ABd, AOpts], Expected),
                    {ok, whatever}
                end),
            ?assertEqual({ok, whatever}, erlang:apply(http_client, request_return_stream, Args))
        end, [Test1, Test2, Test3, Test4, Test5, Test6, Test7]),
    ?assert(meck:validate(hackney)).


% Asserts if the arguments of actual call to hackney are the same as expected.
compare_args(Actual, Expected) ->
    [AMthd, AURL, AHdrs, ABd, AOpts] = Actual,
    [EMthd, EURL, EHdrs, EBd, EOpts] = Expected,
    ?assertEqual(AMthd, EMthd),
    ?assertEqual(AURL, EURL),
    compare_proplists(AHdrs, EHdrs),
    ?assertEqual(ABd, EBd),
    compare_proplists(AOpts, EOpts).


% Asserts if two proplists are equal
% (the order within lists is not important).
compare_proplists(ListA, ListB) ->
    ?assertEqual(length(ListA), length(ListB)),
    lists:foreach(
        fun(Record) ->
            case Record of
                {Key, Val} when is_list(Val) ->
                    compare_proplists(Val, proplists:get_value(Key, ListB));
                _ ->
                    ?assert(lists:member(Record, ListB))
            end
        end, ListA).