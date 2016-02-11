-module(efrisby_constraint).

%% Callbacks
-export([
    evaluate/2
]).

evaluate(Expectations, {ok, Response}) when erlang:is_list(Expectations) ->
    lists:foreach(fun(Expec) -> evaluate(Expec, Response) end, Expectations);

evaluate({content_type, ExpectedContentType}, {_Status, Headers, _Body}) ->
    assert_equal(
        ExpectedContentType,
        proplists:get_value("content-type", Headers)
    );

evaluate({json, ExpectedJson}, Response) ->
   evaluate({json, ".", ExpectedJson}, Response);

evaluate({json, Path, ExpectedJson}, {_Status, _Headers, Body} ) ->
    assert_equal(
        ExpectedJson,
        efrisby_data:get(Path, Body)
    );

evaluate({body_contains, ExpectedBody}, {_Status, _Headers, Body}) ->
    ActualBody = case (efrisby_data:get_type(Body)) of
        string -> Body;
        binary -> erlang:binary_to_list(Body);
        _      -> fail(ExpectedBody, Body)
    end,
    assert_contains(ExpectedBody, ActualBody),
    ok;

evaluate({status, ExpectedStatus}, {{_Version, ActualStatus, _ReasonPhrase}, _Headers, _Body}) ->
    assert_equal(ExpectedStatus, ActualStatus).

assert_equal(Expect, Actual) ->
    case (Actual) of
        Expect -> ok;
        _      -> fail(Expect, Actual)
    end.

assert_contains(Expect, Actual) ->
    case (string:str(Actual, Expect) > 0) of
        true -> ok;
        _    -> fail(Expect, Actual)
    end.

fail(Expect, Actual) ->
    erlang:throw({efrisby_expectation_failed, [
        {expected, Expect},
        {actual, Actual}
    ]}).
