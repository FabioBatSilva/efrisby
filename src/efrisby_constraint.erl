-module(efrisby_constraint).

%% Callbacks
-export([
    evaluate/2
]).

-spec evaluate( tuple() , tuple()) -> 'ok' | none().

evaluate([], {ok, _}) ->
    ok;

evaluate(_Expectations, {error, _} = Error) ->
    fail(ok, Error, {request});

evaluate(Expectations, {ok, Response}) when erlang:is_list(Expectations) ->
    lists:foreach(fun(Expec) -> evaluate(Expec, Response) end, Expectations);

evaluate({content_type, ExpectedContentType}, Response) ->
    assert_equal(ExpectedContentType, efrisby_resp:header("Content-Type", Response), {content_type});

evaluate({json, ExpectedJson}, Response) ->
    evaluate({json, ".", ExpectedJson}, Response);

evaluate({json, RootPath, [{SubPath, ExpectedJson}|_List]}, Response) ->
    evaluate({json, path_concat(RootPath, SubPath), ExpectedJson}, Response);

evaluate({json, Path, ExpectedJson}, Response) ->
    assert_equal(ExpectedJson, efrisby_resp:json(Path, Response), {json, Path});

evaluate({headers, ExpectedHeaders}, Response) ->
    evaluate({headers, ".", ExpectedHeaders}, Response);

evaluate({headers, RootPath, [{SubPath, ExpectedHeaders}|_List]}, Response) ->
    evaluate({headers, path_concat(RootPath, SubPath), ExpectedHeaders}, Response);

evaluate({headers, Name, ExpectedHeaders}, Response) ->
    assert_equal(ExpectedHeaders, efrisby_resp:header(Name, Response), {headers, efrisby_resp:headers(Response)});

evaluate({json_types, ExpectedTypes}, Response) ->
    evaluate({json_types, ".", ExpectedTypes}, Response);

evaluate({json_types, RootPath, [{SubPath, ExpectedType}|_List]}, Response) ->
    ActualPath  = path_concat(RootPath, SubPath),
    ActualValue = efrisby_resp:json(ActualPath, Response),
    ActualType  = case (ActualValue) of
        undefined -> undefined;
        _         -> efrisby_data:type_of(ActualValue)
    end,

    assert_equal(ExpectedType, ActualType, {json_types, ActualPath});

evaluate({body_contains, ExpectedBody}, Response) when erlang:is_binary(ExpectedBody) ->
    evaluate({body_contains, erlang:binary_to_list(ExpectedBody)}, Response);

evaluate({body_contains, ExpectedBody}, Response) ->
    assert_contains(ExpectedBody, erlang:binary_to_list(efrisby_resp:body(Response)), {body_contains});

evaluate({status, ExpectedStatus}, Response) ->
    assert_equal(ExpectedStatus, efrisby_resp:status(Response), {status}).

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
