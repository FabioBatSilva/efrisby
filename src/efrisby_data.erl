-module(efrisby_data).

%% Callbacks
-export([
    encode_headers/1,
    json_encode/1,
    get_type/1,
    get/2
]).

encode_headers(Headers) ->
    Function = fun(Val) -> value_to_list(Val) end,
    Result   = lists:keymap(Function, 2, lists:keymap(Function, 1, Headers)),

    Result.

json_encode(Value) ->
    jiffy:encode(Value).

get(Path, Value) ->
    get(Path, Value, get_type(Value)).

get(Path, Value, binary) ->
    get_data(Path, jiffy:decode(Value));

get(Path, Value, string) ->
    get_data(Path, jiffy:decode(Value));

get(Path, Value, tuple) ->
    get_data(Path, Value).

get_data(".", Value) ->
    Value;

get_data(<<".">>, Value) ->
    Value;

get_data(Path, Value) ->
    Parts = get_path_parts(Path),
    Func  = fun(Part, Data) -> get_data_value(Part, Data) end,

    lists:foldl(Func, Value, Parts).

get_data_value(Key, {List}) ->
    get_data_value(Key, List);

get_data_value(Key, List) ->
    proplists:get_value(Key, List).

get_path_parts(Path) when erlang:is_binary(Path)->
    get_path_parts(erlang:binary_to_list(Path));

get_path_parts(Path) ->
    Parts = string:tokens(Path, "."),
    Func  = fun erlang:list_to_binary/1,

    lists:map(Func, Parts).

value_to_list(Value) when erlang:is_binary(Value) ->
    erlang:binary_to_list(Value);

value_to_list(Value) when erlang:is_integer(Value) ->
    erlang:integer_to_list(Value);

value_to_list(Value) ->
    Value.

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