-module(efrisby_tests).
-include_lib("eunit/include/eunit.hrl").

efrisby_test_() ->
    {setup,
        fun setup/0,
        fun teardown/1,
        [
            fun get_request/0,
            fun put_request/0,
            fun post_request/0
        ]
    }.

setup() ->
    inets:start().

teardown(_) ->
    inets:stop().

get_request() ->
    ?assertEqual(ok, efrisby:get("http://httpbin.org/get?foo=bar", [
        {status, 200},
        {content_type, "application/json"},
        {json_types, ".", [
            {<<"args">>, tuple},
            {<<"args.foo">>, bitstring},
            {<<"url">>, bitstring}
        ]},
        {json, ".args", {
            [
                {<<"foo">>, <<"bar">>}
            ]
        }},
        {json, ".headers", [
            {<<"Host">>, <<"httpbin.org">>}
        ]}
    ])),

    ?assertEqual(ok, efrisby:get("http://httpbin.org/get", [
        {status, 200},
        {headers, [
            {"content-type", "application/json"}
        ]},
        {json_types, [
            {url, bitstring},
            {headers, tuple}
        ]},
        {json, [
            {url, <<"http://httpbin.org/get">>},
            {'headers.Host', <<"httpbin.org">>}
        ]}
    ])).

post_request() ->
    Body = {
        [
            {<<"foo">>,<<"bar">>}
        ]
    },

    Headers = [
        {"Accept", "application/json"},
        {<<"X-Extra-Header">>, <<"FooBar">>}
    ],

    Expectations = [
        {status, 200},
        {content_type, "application/json"},
        {json, ".data", <<"{\"foo\":\"bar\"}">>},
        {json, ".headers.X-Extra-Header", <<"FooBar">>},
        {json, ".headers.Accept", <<"application/json">>},
        {json, ".headers.Content-Type", <<"application/json">>}
    ],

    ?assertEqual(ok, efrisby:post("http://httpbin.org/post", Body, Headers, Expectations)).

put_request() ->
    Body = {
        [
            {<<"foo">>, <<"bar">>}
        ]
    },

    Headers = [
        {"Accept", "application/json"},
        {<<"X-Extra-Header">>, <<"FooBar">>}
    ],

    Expectations = [
        {status, 200},
        {content_type, "application/json"},
        {json, ".data", <<"{\"foo\":\"bar\"}">>},
        {json, ".headers.X-Extra-Header", <<"FooBar">>},
        {json, ".headers.Accept", <<"application/json">>},
        {json, ".headers.Content-Type", <<"application/json">>}
    ],

    ?assertEqual(ok, efrisby:put("http://httpbin.org/put", Body, Headers, Expectations)).