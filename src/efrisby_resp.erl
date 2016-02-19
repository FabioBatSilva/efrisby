-module(efrisby_resp).

%% Callbacks
-export([
    headers/1,
    header/2,
    status/1,
    body/1,
    json/1,
    json/2
]).

header(Name, Response) when erlang:is_list(Name) ->
    header(erlang:list_to_binary(Name), Response);

header(Name, Response) ->
    Headers = headers(Response),
    Header  = proplists:get_value(Name, Headers),

    Header.

headers({ok, Response}) ->
    headers(Response);

headers({_Status, Headers, _Body}) ->
    Headers.

body({ok, Response}) ->
    body(Response);

body({_Status, _Headers, Body}) ->
    Body.

json(Path, {ok, Response}) ->
    json(Path, Response);

json(Path, Response) ->
    efrisby_data:get(Path, body(Response)).

json({ok, Response}) ->
    json(Response);

json(Response) ->
    efrisby_data:get(".", body(Response)).

status({ok, Response}) ->
    status(Response);

status({StatusCode, _Headers, _Body}) ->
    StatusCode.
