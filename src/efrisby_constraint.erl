-module(efrisby_constraint).

%% Callbacks
-export([
    evaluate/2
]).

evaluate(Expectations, {ok, Response}) when erlang:is_list(Expectations) ->
    lists:foreach(fun(Expec) -> evaluate(Expec, Response) end, Expectations);

evaluate({content_type, ExpectedContentType} = Ctx, {_Status, Headers, _Body}) ->
    assert_equal(ExpectedContentType, proplists:get_value("content-type", Headers), Ctx);

evaluate({json, ExpectedJson}, Response) ->
    evaluate({json, ".", ExpectedJson}, Response);

evaluate({json, RootPath, [{SubPath, ExpectedJson}|_List]}, Response) ->
    evaluate({json, path_concat(RootPath, SubPath), ExpectedJson}, Response);

evaluate({json, Path, ExpectedJson}, {_Status, _Headers, Body} ) ->
    assert_equal(ExpectedJson, efrisby_data:get(Path, Body), {json, Path});

evaluate({json_types, RootPath, [{SubPath, ExpectedType}|_List]}, {_Status, _Headers, Body} ) ->
    ActualPath  = path_concat(RootPath, SubPath),
    ActualValue = efrisby_data:get(ActualPath, Body),
    ActualType  = case (ActualValue) of
        undefined -> undefined;
        _         -> efrisby_data:type_of(ActualValue)
    end,

    assert_equal(ExpectedType, ActualType, {json_types, ActualPath});

evaluate({body_contains, ExpectedBody}, {_Status, _Headers, Body}) ->
    ActualBody = case (efrisby_data:get_type(Body)) of
        string -> Body;
        binary -> erlang:binary_to_list(Body);
        _      -> fail(ExpectedBody, Body, {body_contains})
    end,

    assert_contains(ExpectedBody, ActualBody, {body_contains});

evaluate({status, ExpectedStatus}, {{_Version, ActualStatus, _ReasonPhrase}, _Headers, _Body}) ->
    assert_equal(ExpectedStatus, ActualStatus, {status}).

path_concat(RootPath, SubPath) when erlang:is_binary(RootPath) ->
    path_concat(erlang:binary_to_list(RootPath), SubPath);

path_concat(RootPath, SubPath) when erlang:is_binary(SubPath) ->
    path_concat(RootPath, erlang:binary_to_list(SubPath));

path_concat(".", SubPath) ->
    SubPath;

path_concat(RootPath, ".") ->
    RootPath;

path_concat(RootPath, SubPath) ->
    lists:concat([RootPath, '.', SubPath]).

assert_equal(Expect, Actual, Context) ->
    case (Actual) of
        Expect -> ok;
        _      -> fail(Expect, Actual, Context)
    end.

assert_contains(Expect, Actual, Context) ->
    case (string:str(Actual, Expect) > 0) of
        true -> ok;
        _    -> fail(Expect, Actual, Context)
    end.

fail(Expect, Actual, Context) ->
    erlang:throw({efrisby_expectation_failed, [
        {context, Context},
        {expected, Expect},
        {actual, Actual}
    ]}).
