-module(efrisby).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Callbacks
-export([
    get/2,
    send/2,
    send/1,
    assert/2
]).

%% records

-record(context, {
    host
}).

-record(request, {
    body,
    host,
    method,
    headers,
    expectations
}).

send(#request{} = Request) ->
    efrisby:send(Request, #context{}).

send(#request{method=Method, host=Host, expectations=Expectations}, #context{} = _Context) ->
    Response = httpc:request(Method, {Host, []}, [], []),
    Result   = efrisby:assert(Expectations, Response),
    Result.

get(Host, Expectations) ->
    efrisby:send(#request{
        method=get,
        host=Host,
        expectations=Expectations}).

assert(Expectations, {ok, Response}) when erlang:is_list(Expectations) ->
    lists:foreach(fun(Expec) -> efrisby:assert(Expec, Response) end, Expectations);

assert({content_type, ExpectedContentType}, {_Status, Headers, _Body}) ->
    ?assertEqual(
        ExpectedContentType,
        proplists:get_value("content-type", Headers)
    );

assert({json, ExpectedJson}, Response) ->
   assert({json, ".", ExpectedJson}, Response);

assert({json, Path, ExpectedJson}, {_Status, _Headers, Body} ) ->
    ?assertEqual(
        ExpectedJson,
        efrisby_json:get(Path, Body)
    );

assert({body_contains, ExpectedBody}, {_Status, _Headers, Body}) ->
    erlang:display(ExpectedBody),
    erlang:display(Body),
    ok;

assert({status, ExpectedStatus}, {{_Version, ActualStatus, _ReasonPhrase}, _Headers, _Body}) ->
    ?assertEqual(ExpectedStatus, ActualStatus).


%% endif (TEST).
-endif.