-module(efrisby_tests).
-include_lib("eunit/include/eunit.hrl").

get_request_test() ->
    ?assertMatch({ok,_}, efrisby:get("http://httpbin.org/get?foo=bar", [
        {status, 200},
        {content_type, "application/json"},
        {json_types, ".", [
            {<<"args">>, list},
            {<<"url">>, bitstring},
            {<<"args.foo">>, bitstring}
        ]},
        {json, ".args",[
            {<<"foo">>, <<"bar">>}
        ]},
        {json, ".headers", [
            {<<"Host">>, <<"httpbin.org">>}
        ]}
    ])),

    {ok, Response} = efrisby:get("http://httpbin.org/get", [
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
    ]),

    ?assertMatch({{_Version, _Status, _ReasonPhrase}, _Headers, _Body}, Response).

post_request_test() ->
    Body = [
        {<<"foo">>,<<"bar">>}
    ],

    Options = [
        {headers , [
            {"Accept", "application/json"},
            {<<"X-Extra-Header">>, <<"FooBar">>}
        ]}
    ],

    Expectations = [
        {status, 200},
        {content_type, "application/json"},
        {json, ".data", <<"{\"foo\":\"bar\"}">>},
        {json, ".headers.X-Extra-Header", <<"FooBar">>},
        {json, ".headers.Accept", <<"application/json">>},
        {json, ".headers.Content-Type", <<"application/json">>}
    ],

    ?assertMatch({ok,_}, efrisby:post("http://httpbin.org/post", Body, Expectations, Options)).

put_request_test() ->
    Body = [
        {<<"foo">>, <<"bar">>}
    ],

    Options = [
        {headers , [
            {"Accept", "application/json"},
            {<<"X-Extra-Header">>, <<"FooBar">>}
        ]}
    ],

    Expectations = [
        {status, 200},
        {content_type, "application/json"},
        {json, ".data", <<"{\"foo\":\"bar\"}">>},
        {json, ".headers.X-Extra-Header", <<"FooBar">>},
        {json, ".headers.Accept", <<"application/json">>},
        {json, ".headers.Content-Type", <<"application/json">>}
    ],

    ?assertMatch({ok,_}, efrisby:put("http://httpbin.org/put", Body, Expectations, Options)).

delete_request_test() ->
    ?assertMatch({ok,_}, efrisby:delete("http://httpbin.org/delete?foo=bar", [
        {status, 200},
        {json, ".args.foo", <<"bar">>},
        {content_type, "application/json"},
        {json, ".headers.Host", <<"httpbin.org">>}
    ])).

options_request_test() ->
    ?assertMatch({ok,_}, efrisby:options("http://httpbin.org/get", [
        {status, 200}
    ])).


context_options_request_test() ->

    Args = [
        {<<"foo">>, <<"bar">>}
    ],

    Config = [
        {base_url, "http://httpbin.org"},
        {headers , [
            {"Accept", "application/json"},
            {<<"X-Extra-Header">>, <<"FooBar">>}
        ]}
    ],

    Options = [
        {config, Config}
    ],

    Expectations = [
        {status, 200},
        {json, ".args", Args},
        {content_type, "application/json"},
        {json, ".headers.X-Extra-Header", <<"FooBar">>},
        {json, ".headers.Accept", <<"application/json">>}
    ],

    ?assertMatch({ok,_}, efrisby:get("/get?foo=bar", Expectations, Options)).