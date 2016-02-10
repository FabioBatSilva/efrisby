-module(efrisby_json).

%% Callbacks
-export([
    get/2,
    get_data/2
]).

get(Path, Body) when erlang:is_binary(Body) ->
    get(Path, jiffy:decode(Body));

get(Path, Body) ->
    case is_string(Body) of
        true -> get(Path, jiffy:decode(Body));
        _    -> get_data(Path, Body)
    end.

get_data(".", Json) ->
    Json;

get_data(<<".">>, Json) ->
    Json;

get_data(Path, Json) ->
    Parts = get_path_parts(Path),
    Func  = fun(Part, Data) -> get_value(Part, Data) end,

    lists:foldl(Func, Json, Parts).

get_value(Key, {List}) when erlang:is_list(Key) ->
    get_value(erlang:list_to_binary(Key), List);

get_value(Key, List) ->
    proplists:get_value(Key, List).

get_path_parts(Path) when erlang:is_binary(Path)->
    get_path_parts(erlang:binary_to_list(Path));

get_path_parts(Path) ->
    case is_string(Path) of
        true -> string:tokens(Path, ".");
        _    -> Path
    end.

is_string([]) -> true;
is_string([X|T]) -> is_integer(X) andalso X>=0 andalso is_string(T);
is_string(_) -> false.
