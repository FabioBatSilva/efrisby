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

    send/4,
    send/5
]).

put(Url, Expectations) ->
    put(Url, null, Expectations).

put(Url, Body, Expectations) ->
    put(Url, Body, Expectations, []).

put(Url, Body, Expectations, Options) ->
    send(put, Url, Body, Expectations, Options).

post(Url, Expectations) ->
    post(Url, null, Expectations).

post(Url, Body, Expectations) ->
    post(Url, Body, Expectations, []).

post(Url, Body, Expectations, Options) ->
    send(post, Url, Body, Expectations, Options).

patch(Url, Expectations) ->
    patch(Url, null, Expectations).

patch(Url, Body, Expectations) ->
    patch(Url, Body, Expectations, []).

patch(Url, Body, Expectations, Options) ->
    send(patch, Url, Body, Expectations, Options).

delete(Url, Expectations) ->
    delete(Url, null, Expectations).

delete(Url, Body, Expectations) ->
    delete(Url, Body, Expectations, []).

delete(Url, Body, Expectations, Options) ->
    send(delete, Url, Body, Expectations, Options).

get(Url, Expectations) ->
    get(Url, Expectations, []).

get(Url, Expectations, Options) ->
    send(get, Url, Expectations, Options).

head(Url, Expectations) ->
    head(Url, Expectations, []).

head(Url, Expectations, Options) ->
    send(head, Url, Expectations, Options).

options(Url, Expectations) ->
    options(Url, Expectations, []).

options(Url, Expectations, Options) ->
    send(options, Url, Expectations, Options).

send(Method, Url, Expectations, Options) ->
    send(Method, Url, null, Expectations, Options).

send(Method, Url, Body, Expectations, Options) ->
    BaseUrl  = proplists:get_value(base_url, Options),
    Uri      = request_uri(BaseUrl, Url),
    Headers  = request_headers(Options),
    Payload  = request_payload(Body),

    Response = send_request(Method, Uri,  Headers, Payload, Options),
    ok       = efrisby_constraint:evaluate(Expectations, Response),

    Response.

send_request(Method, Uri,  Headers, Payload, _Options) ->
    {ok, Status, RespHeaders, Ref} = hackney:request(Method, Uri,  Headers, Payload, []),
    {ok, Body}                     = hackney:body(Ref),

    {ok, {Status, RespHeaders, Body}}.

request_payload(Body) ->
    case (Body) of
        null -> <<"">>;
        _    -> efrisby_data:json_encode(Body)
    end.

request_headers(Options) ->
    Defaults = [{"User-Agent", "erlang/efrisby"}, {"Content-Type", "application/json"}],
    Headers  = proplists:get_value(headers, Options, []),
    Merged   = merge_proplists(Defaults, Headers),

    efrisby_data:encode_headers(Merged).

request_uri(undefined, Url) ->
    Url;

request_uri(BaseUrl, Url) ->
    case (http_uri:parse(Url)) of
        {ok, _} -> Url;
        _       -> lists:concat([BaseUrl, Url])
    end.

merge_proplists(Values, Overrides) ->
    Keys = proplists:get_keys(Values ++ Overrides),
    Func = fun(Key, List) ->
        Default = proplists:get_value(Key, Values),
        Element = proplists:get_value(Key, Overrides, Default),

        [{Key, Element} | List]
    end,

    lists:foldl(Func, [], Keys).