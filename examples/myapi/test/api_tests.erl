-module(api_tests).
-include_lib("eunit/include/eunit.hrl").

-define(API_CONFIG, [
    {base_url, "http://localhost:8080"},
    {headers , [
        {"Accept", "application/json"}
    ]}
]).

api_test_() ->
    {setup,
        fun setup/0,
        fun teardown/1,
        [
            fun get_request/0,
            fun post_request/0
        ]
    }.

setup() ->
    application:start(inets).

teardown(_) ->
    application:stop(inets).

get_request() ->
    ?assertMatch({ok,_}, efrisby:get("http://localhost:8080/", [
        {status, 200},
        {content_type, "application/json"}
    ], ?API_CONFIG)).

post_request() ->
    Body = {
        [
            {<<"foo">>,<<"bar">>}
        ]
    },

    Expectations = [
        {status, 200},
        {content_type, "application/json"},
        {headers, [
            {"content-type", "application/json"}
        ]},
        {json_types, [
            {foo, bitstring}
        ]},
        {json, [
            {foo, <<"bar">>}
        ]}
    ],

    ?assertMatch({ok,_}, efrisby:post("/", Body, Expectations, ?API_CONFIG)).