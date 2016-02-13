-module(efrisby).

%% Callbacks
-export([
    get/2,
    get/3,

    put/2,
    put/3,
    put/4,

    head/2,
    head/3,

    post/2,
    post/3,
    post/4,

    patch/2,
    patch/3,
    patch/4,

    delete/2,
    delete/3,
    delete/4,

    options/2,
    options/3,

    send/2,
    send/3
]).

put(Url, Expectations) ->
    put(Url, null, Expectations).

put(Url, Body, Expectations) ->
    put(Url, Body, Expectations, []).

put(Url, Body, Expectations, Options) ->
    send(create_options([
        {url, Url},
        {body, Body},
        {method, put}
    ], Options), Expectations).

post(Url, Expectations) ->
    post(Url, null, Expectations).

post(Url, Body, Expectations) ->
    post(Url, Body, Expectations, []).

post(Url, Body, Expectations, Options) ->
    send(create_options([
        {url, Url},
        {body, Body},
        {method, post}
    ], Options), Expectations).

patch(Url, Expectations) ->
    patch(Url, null, Expectations).

patch(Url, Body, Expectations) ->
    patch(Url, Body, Expectations, []).

patch(Url, Body, Expectations, Options) ->
    send(create_options([
        {url, Url},
        {body, Body},
        {method, patch}
    ], Options), Expectations).

delete(Url, Expectations) ->
    delete(Url, null, Expectations).

delete(Url, Body, Expectations) ->
    delete(Url, Body, Expectations, []).

delete(Url, Body, Expectations, Options) ->
    send(create_options([
        {url, Url},
        {body, Body},
        {method, delete}
    ], Options), Expectations).

get(Url, Expectations) ->
    get(Url, Expectations, []).

get(Url, Expectations, Options) ->
    send(create_options([
        {url, Url},
        {method, get}
    ], Options), Expectations).

head(Url, Expectations) ->
    head(Url, Expectations, []).

head(Url, Expectations, Options) ->
    send(create_options([
        {url, Url},
        {method, head}
    ], Options), Expectations).

options(Url, Expectations) ->
    options(Url, Expectations, []).

options(Url, Expectations, Options) ->
    send(create_options([
        {url, Url},
        {method, options}
    ], Options), Expectations).

send(Options, Expectations) ->
    send(Options, Expectations, []).

send(Options, Expectations, _Context) ->
    Url         = proplists:get_value(url, Options),
    Method      = proplists:get_value(method, Options),
    Body        = proplists:get_value(body, Options, null),
    Headers     = proplists:get_value(headers, Options, []),
    ContentType = proplists:get_value(content_type, Options, "application/json"),
    Request     = create_request(Url, Body, Headers, ContentType),
    Response    = httpc:request(Method, Request, [], []),
    Result      = efrisby_constraint:evaluate(Expectations, Response),

    case (Result) of
        ok -> Response;
        _  -> Result
    end.

create_options(Options, Overrides) ->
    Keys = proplists:get_keys(Options ++ Overrides),
    Func = fun(Key, List) ->
        Default = proplists:get_value(Key, Options),
        Element = proplists:get_value(Key, Overrides, Default),

        [{Key, Element} | List]
    end,

    lists:foldl(Func, [], Keys).

create_request(Url, null, [], _ContentType) ->
    {Url, []};

create_request(Url, null, Headers, _ContentType) ->
    {Url, Headers};

create_request(Url, Body, Headers, ContentType) ->
    {
        Url,
        efrisby_data:encode_headers(Headers),
        ContentType,
        efrisby_data:json_encode(Body)
    }.
