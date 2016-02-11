-module(efrisby_json).

%% Callbacks
-export([
    get/2,
    encode/1
]).

encode(Body) ->
    jiffy:encode(Body).

get(Path, Body) ->
    get(Path, Body, get_type(Body)).

get(Path, Body, binary) ->
    get_data(Path, jiffy:decode(Body));

get(Path, Body, string) ->
    get_data(Path, jiffy:decode(Body));

get(Path, Body, tuple) ->
    get_data(Path, Body).

get_data(".", Json) ->
    Json;

get_data(<<".">>, Json) ->
    Json;

get_data(Path, Json) ->
    Parts = get_path_parts(Path),
    Func  = fun(Part, Data) -> get_value(Part, Data) end,

    lists:foldl(Func, Json, Parts).

get_value(Key, {List}) ->
    get_value(Key, List);

get_value(Key, List) ->
    proplists:get_value(Key, List).

get_path_parts(Path) when erlang:is_binary(Path)->
    get_path_parts(erlang:binary_to_list(Path));

get_path_parts(Path) ->
    Parts = string:tokens(Path, "."),
    Func  = fun erlang:list_to_binary/1,

    lists:map(Func, Parts).

get_type(Value) when erlang:is_binary(Value) ->
    binary;

get_type({_}) ->
    tuple;

get_type(Value) when erlang:is_list(Value) ->
    case is_string(Value) of
        true -> string;
        _    -> list
    end.

is_string([]) -> true;
is_string([X|T]) -> is_integer(X) andalso X>=0 andalso is_string(T);
is_string(_) -> false.
