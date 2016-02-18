%% @doc rest-api handler.
-module(myapi_handler).

-export([
    init/3,
    allowed_methods/2,
    handle_get_request/2,
    handle_post_request/2,
    content_types_provided/2,
    content_types_accepted/2
]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{'*', handle_post_request}], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_get_request}
    ], Req, State}.

handle_get_request(Req, State) ->
    {response_body(Req), Req, State}.

handle_post_request(Req, State) ->
    Body = response_body(Req),
    Resp = cowboy_req:set_resp_body(Body, Req),

    {true, Resp, State}.

response_body(Req) ->
    {Url, _}       = cowboy_req:url(Req),
    {ok, Body, _}  = cowboy_req:body(Req),
    {Headers, _}   = cowboy_req:headers(Req),
    JsonBody       = case (jsx:is_json(Body)) of
        true -> jsx:decode(Body);
        _    -> null
    end,

    jsx:encode([
        {<<"url">>, Url},
        {<<"json">>, JsonBody},
        {<<"headers">>, Headers}
    ]).
