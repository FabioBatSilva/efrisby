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

    send/3
]).

put(Url, Expectations) ->
    put(Url, null, Expectations).

put(Url, Body, Expectations) ->
    put(Url, Body, Expectations, []).

put(Url, Body, Expectations, Opts) ->
    Config  = get_config(Opts),
    Options = merge_proplists([
        {url, Url},
        {body, Body},
        {method, put}
    ], Opts),

    send(Options, Expectations, Config).

post(Url, Expectations) ->
    post(Url, null, Expectations).

post(Url, Body, Expectations) ->
    post(Url, Body, Expectations, []).

post(Url, Body, Expectations, Opts) ->
    Config  = get_config(Opts),
    Options = merge_proplists([
        {url, Url},
        {body, Body},
        {method, post}
    ], Opts),

    send(Options, Expectations, Config).

patch(Url, Expectations) ->
    patch(Url, null, Expectations).

patch(Url, Body, Expectations) ->
    patch(Url, Body, Expectations, []).

patch(Url, Body, Expectations, Opts) ->
    Config  = get_config(Opts),
    Options = merge_proplists([
        {url, Url},
        {body, Body},
        {method, patch}
    ], Opts),

    send(Options, Expectations, Config).

delete(Url, Expectations) ->
    delete(Url, null, Expectations).

delete(Url, Body, Expectations) ->
    delete(Url, Body, Expectations, []).

delete(Url, Body, Expectations, Opts) ->
    Config  = get_config(Opts),
    Options = merge_proplists([
        {url, Url},
        {body, Body},
        {method, delete}
    ], Opts),

    send(Options, Expectations, Config).

get(Url, Expectations) ->
    get(Url, Expectations, []).

get(Url, Expectations, Opts) ->
    Config  = get_config(Opts),
    Options = merge_proplists([
        {url, Url},
        {method, get}
    ], Opts),

    send(Options, Expectations, Config).

head(Url, Expectations) ->
    head(Url, Expectations, []).

head(Url, Expectations, Opts) ->
    Config  = get_config(Opts),
    Options = merge_proplists([
        {url, Url},
        {method, head}
    ], Opts),

    send(Options, Expectations, Config).

options(Url, Expectations) ->
    options(Url, Expectations, []).

options(Url, Expectations, Opts) ->
    Config  = get_config(Opts),
    Options = merge_proplists([
        {url, Url},
        {method, options}
    ], Opts),

    send(Options, Expectations, Config).

send(Options, Expectations, Config) ->
    Request  = create_request(Options, Config),
    Method   = proplists:get_value(method, Options),
    Response = httpc:request(Method, Request, [], []),
    ok       = efrisby_constraint:evaluate(Expectations, Response),

    Response.

create_request(Options, Config) ->
    Url         = get_url_option(Options, Config),
    Headers     = get_headers_option(Options, Config),
    Body        = proplists:get_value(body, Options, null),
    ContentType = get_option(content_type, Options, Config, "application/json"),

    case (Body) of
        null -> {Url, Headers};
        _    -> {
            Url,
            Headers,
            ContentType,
            efrisby_data:json_encode(Body)
        }
    end.

get_config(Options) ->
    proplists:get_value(config, Options, []).

get_option(Name, Options, Config, Default) ->
    ConfigHeaders  = proplists:get_value(Name, Config, Default),
    OptionsHeaders = proplists:get_value(Name, Options, ConfigHeaders),

    OptionsHeaders.

get_url_option(Options, Config) ->
    Url     = proplists:get_value(url, Options),
    BaseUrl = proplists:get_value(base_url, Config, null),

    create_request_url(BaseUrl, Url).

get_headers_option(Options, Config) ->
    Values    = [{"User-Agent", "erlang/efrisby"}],
    Overrides = get_option(headers, Options, Config, []),
    Headers   = efrisby_data:encode_headers(merge_proplists(Values, Overrides)),

    Headers.

create_request_url(null, Url) ->
    Url;

create_request_url(BaseUrl, Url) ->
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