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
    hackney:start().

teardown(_) ->
    hackney:stop().

get_request() ->
    ?assertMatch({ok,_}, efrisby:get("/", [
        {status, 200},
        {content_type, <<"application/json">>},
        {json, [
            {url, <<"http://localhost:8080/">>}
        ]}
    ], ?API_CONFIG)).

post_request() ->
    Json = [
        {<<"foo">>, <<"bar">>}
    ],

    Expectations = [
        {status, 200},
        {content_type, <<"application/json">>},
        {headers, [
            {<<"content-type">>, <<"application/json">>}
        ]},
        {json_types, [
            {json, list}
        ]},
        {json, [
            {json, Json},
            {"headers.host", <<"localhost:8080">>}
        ]}
    ],

    ?assertMatch({ok,_}, efrisby:post("/", Json, Expectations, ?API_CONFIG)).