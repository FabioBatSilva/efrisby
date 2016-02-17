%% @doc rest-api handler.
-module(myapi_handler).

-export([
    init/3,
    handle_request/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2
]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[
        <<"GET">>,
        <<"HEAD">>,
        <<"POST">>,
        <<"PUT">>,
        <<"PATCH">>,
        <<"DELETE">>,
        <<"OPTIONS">>
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, handle_request}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_request}
    ], Req, State}.

handle_request(Req, State) ->
    {ok, Body, _} = cowboy_req:body(Req),

    {Body, Req, State}.
