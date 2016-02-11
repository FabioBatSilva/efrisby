-module(efrisby).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Callbacks
-export([
    get/2,
    get/3,
    post/3,
    post/4,
    send/2,
    send/1,
    assert/2
]).

%% records

-record(context, {
    host
}).

-record(request, {
    host,
    method,
    body = null,
    headers = [],
    expectations = []
}).

get(Host, Expectations) ->
    send(get, Host, null, [], Expectations).

get(Host, Headers, Expectations) ->
    send(get, Host, null, Headers, Expectations).

post(Host, Body, Expectations) ->
    send(post, Host, Body, [], Expectations).

post(Host, Body, Headers, Expectations) ->
    send(post, Host, Body, Headers, Expectations).

send(Method, Host, Body, Headers, Expectations) ->
    send(#request{
        expectations=Expectations,
        headers=Headers,
        method=Method,
        host=Host,
        body=Body
    }).

send(#request{} = Request) ->
    send(Request, #context{}).

send(#request{method=Method, expectations=Expectations} = Request, #context{} = Context) ->
    Response = httpc:request(Method, create_request(Request, Context), [], []),
    Result   = assert(Expectations, Response),
    Result.

assert(Expectations, {ok, Response}) when erlang:is_list(Expectations) ->
    lists:foreach(fun(Expec) -> assert(Expec, Response) end, Expectations);

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

create_request(#request{host=Host, headers=[], body=null}, #context{} = _Context) ->
    {Host, []};

create_request(#request{ host=Host, headers=Headers, body=Body}, #context{} = _Context) ->
    {
        Host,
        efrisby_converter:encode_headers(Headers),
        "application/json",
        efrisby_json:encode(Body)
    }.

%% endif (TEST).
-endif.