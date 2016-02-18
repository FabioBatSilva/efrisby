-module(api_tests).
-include_lib("eunit/include/eunit.hrl").

-define(API_CONFIG, [
    {base_url, "http://localhost:8080"},
    {headers , [
        {"Accept", "application/json"}
    ]}
]).

get_request_test() ->
    ?assertMatch({ok,_}, efrisby:get("http://localhost:8080/", [
        {status, 200},
        {content_type, "application/json"},
        {json, [
            {url, <<"http://localhost:8080/">>}
        ]}
    ], ?API_CONFIG)).

post_request_test() ->
    Json = [
        {<<"foo">>, <<"bar">>}
    ],

    Expectations = [
        {status, 200},
        {content_type, "application/json"},
        {headers, [
            {"content-type", "application/json"}
        ]},
        {json_types, [
            {json, list}
        ]},
        {json, [
            {json, Json},
            {"headers.host", <<"localhost:8080">>}
        ]}
    ],

    ?assertMatch({ok,_}, efrisby:post("http://localhost:8080/", Json, Expectations, ?API_CONFIG)).