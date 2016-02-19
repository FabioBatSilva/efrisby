-module(efrisby_constraint_tests).

-include_lib("eunit/include/eunit.hrl").

evaluate_content_type_test() ->

    ExpectationOk   = {content_type, <<"application/json">>},
    ExpectationFail = {content_type, <<"application/xml">>},
    Response        = {
        200,
        [ {<<"Content-Type">>, <<"application/json">>} ],
        <<"[1,2,3]">>
    },

    ExpectedException = {efrisby_expectation_failed, [
        {context,{content_type}},
        {expected,<<"application/xml">>},
        {actual,<<"application/json">>}
    ]},

    ?assertEqual(ok, efrisby_constraint:evaluate(ExpectationOk, Response)),
    ?assertThrow(ExpectedException, efrisby_constraint:evaluate(ExpectationFail, Response)).

evaluate_status_test() ->

    ExpectationOk   = {status, 200},
    ExpectationFail = {status, 202},
    Response        = {
        200,
        [ {<<"Content-Type">>, <<"application/json">>} ],
        <<"[1,2,3]">>
    },
    ExpectedException = {efrisby_expectation_failed,[
        {context,{status}},
        {expected,202},
        {actual,200}
    ]},

    ?assertEqual(ok, efrisby_constraint:evaluate(ExpectationOk, Response)),
    ?assertThrow(ExpectedException, efrisby_constraint:evaluate(ExpectationFail, Response)).

evaluate_json_test() ->

    ExpectationOk   = {json, [1,2,3]},
    ExpectationFail = {json, ".unknown", [1,2,3]},
    Response        = {
        200,
        [ {<<"Content-Type">>, <<"application/json">>} ],
        <<"[1,2,3]">>
    },
    ExpectedException = {efrisby_expectation_failed,[
        {context,{json,".unknown"}},
        {expected,[1,2,3]},
        {actual,undefined}
    ]},

    ?assertEqual(ok, efrisby_constraint:evaluate(ExpectationOk, Response)),
    ?assertThrow(ExpectedException, efrisby_constraint:evaluate(ExpectationFail, Response)).

evaluate_json_types_test() ->
    ExpectationOk   = {json_types, ".", [
        {<<"args">>, list},
        {<<"args.foo">>, bitstring}
    ]},
    ExpectationFail = {json_types, ".", [
        {<<"args">>, integer}
    ]},
    Response        = {
        200,
        [ {<<"Content-Type">>, <<"application/json">>} ],
        <<"{\"args\":{\"foo\":\"bar\"}}">>
    },
    ExpectedException = {efrisby_expectation_failed, [
        {context,{json_types,"args"}},
        {expected,integer},
        {actual,list}
    ]},

    ?assertEqual(ok, efrisby_constraint:evaluate(ExpectationOk, Response)),
    ?assertThrow(ExpectedException, efrisby_constraint:evaluate(ExpectationFail, Response)).

evaluate_body_contains_test() ->

    ExpectationOk   = {body_contains, "OK"},
    ExpectationFail = {body_contains, "NOT-OK"},
    Response        = {
        200,
        [ {<<"Content-Type">>, <<"text/plain">>} ],
        <<"OK,OK,OK">>
    },
    ExpectedException = {efrisby_expectation_failed,[
        {context,{body_contains}},
        {expected, "NOT-OK"},
        {actual, "OK,OK,OK"}
    ]},

    ?assertEqual(ok, efrisby_constraint:evaluate(ExpectationOk, Response)),
    ?assertThrow(ExpectedException, efrisby_constraint:evaluate(ExpectationFail, Response)).

evaluate_expectation_list_test() ->

    ExpectationOk   = [{status, 200}],
    ExpectationFail = [{status, 202}],
    Response        = {
        ok,
        {
            200,
            [ ],
            <<"OK">>
        }
    },
    ExpectedException = {efrisby_expectation_failed,[
        {context,{status}},
        {expected,202},
        {actual,200}
    ]},

    ?assertEqual(ok, efrisby_constraint:evaluate(ExpectationOk, Response)),
    ?assertThrow(ExpectedException, efrisby_constraint:evaluate(ExpectationFail, Response)).