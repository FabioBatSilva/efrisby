-module(efrisby_resp_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SIMPLE_BODY, <<"{\"foo\":\"bar\"}">>).

-define(SIMPLE_RESPONSE, {
    200,
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Content-Length">>, <<"13">>}
    ],
    ?SIMPLE_BODY
}).

-define(SIMPLE_RESULT, {ok, ?SIMPLE_RESPONSE}).

status_test() ->
    Response = ?SIMPLE_RESPONSE,
    Result   = ?SIMPLE_RESULT,

    ?assertEqual(200, efrisby_resp:status(Response)),
    ?assertEqual(200, efrisby_resp:status(Result)).

body_test() ->
    Response = ?SIMPLE_RESPONSE,
    Result   = ?SIMPLE_RESULT,

    ?assertEqual(?SIMPLE_BODY, efrisby_resp:body(Response)),
    ?assertEqual(?SIMPLE_BODY, efrisby_resp:body(Result)).

json_test() ->
    Response = ?SIMPLE_RESPONSE,
    Result   = ?SIMPLE_RESULT,

    ?assertEqual([{<<"foo">>, <<"bar">>}], efrisby_resp:json(Response)),
    ?assertEqual([{<<"foo">>, <<"bar">>}], efrisby_resp:json(Result)),

    ?assertEqual(<<"bar">>, efrisby_resp:json(".foo", Response)),
    ?assertEqual(<<"bar">>, efrisby_resp:json(".foo", Result)),

    ?assertEqual(<<"bar">>, efrisby_resp:json("foo", Response)),
    ?assertEqual(<<"bar">>, efrisby_resp:json("foo", Result)).

headers_test() ->
    Response = ?SIMPLE_RESPONSE,
    Result   = ?SIMPLE_RESULT,
    Expected = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Content-Length">>, <<"13">>}
    ],

    ?assertEqual(Expected, efrisby_resp:headers(Response)),
    ?assertEqual(Expected, efrisby_resp:headers(Result)).


header_test() ->
    Response = ?SIMPLE_RESPONSE,
    Result   = ?SIMPLE_RESULT,

    ?assertEqual(<<"application/json">>, efrisby_resp:header("Content-Type", Response)),
    ?assertEqual(<<"application/json">>, efrisby_resp:header("Content-Type", Result)),

    ?assertEqual(<<"13">>, efrisby_resp:header(<<"Content-Length">>, Response)),
    ?assertEqual(<<"13">>, efrisby_resp:header(<<"Content-Length">>, Result)).
