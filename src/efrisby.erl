-module(efrisby).

%% Callbacks
-export([
    get/2,
    get/3,
    post/3,
    post/4,
    send/2,
    send/1
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
    Result   = efrisby_constraint:evaluate(Expectations, Response),
    Result.

create_request(#request{host=Host, headers=[], body=null}, #context{} = _Context) ->
    {Host, []};

create_request(#request{ host=Host, headers=Headers, body=Body}, #context{} = _Context) ->
    {
        Host,
        efrisby_data:encode_headers(Headers),
        "application/json",
        efrisby_data:json_encode(Body)
    }.
