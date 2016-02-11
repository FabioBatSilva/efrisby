-module(efrisby_tests).
-include_lib("eunit/include/eunit.hrl").

get_request_test() ->
    inets:start(),

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
    ])).

post_request_test() ->
    inets:start(),

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