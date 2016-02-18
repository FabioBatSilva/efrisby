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

header(Name, Response) when erlang:is_binary(Name) ->
    header(erlang:binary_to_list(Name), Response);

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
    case (erlang:is_binary(Body)) of
        true -> erlang:binary_to_list(Body);
        _    -> Body
    end.

json(Path, {ok, Response}) ->
    json(Path, Response);

json(Path, {_Status, _Headers, Body}) ->
    efrisby_data:get(Path, Body).

json({ok, Response}) ->
    json(Response);

json({_Status, _Headers, Body}) ->
    efrisby_data:get(".", Body).

status({ok, Response}) ->
    status(Response);

status({{_Version, StatusCode, _ReasonPhrase}, _Headers, _Body}) ->
    StatusCode.
