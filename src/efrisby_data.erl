-module(efrisby_data).

%% Callbacks
-export([
    encode_headers/1,
    json_encode/1,
    get_type/1,
    type_of/1,
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

get(Path, Value, bitstring) ->
    get_data(Path, jiffy:decode(Value));

get(Path, Value, string) ->
    get_data(Path, jiffy:decode(Value));

get(Path, Value, tuple) ->
    get_data(Path, Value);

get(Path, Value, list) ->
    get_data(Path, Value).

get_data(".", Value) ->
    Value;

get_data(<<".">>, Value) ->
    Value;

get_data(Path, Value) when erlang:is_list(Value) ->
    get_data_value(Path, Value);

get_data(Path, Value) ->
    Parts = get_path_parts(Path),
    Func  = fun(Part, Data) -> get_data_value(Part, Data) end,

    lists:foldl(Func, Value, Parts).

get_data_value(Key, {List}) ->
    get_data_value(Key, List);

get_data_value(Key, List) ->
    case (is_integer_binary(Key)) of
        true -> lists:nth(erlang:binary_to_integer(Key), List);
        _    -> proplists:get_value(Key, List)
    end.

get_path_parts(Path) when erlang:is_bitstring(Path)->
    get_path_parts(erlang:binary_to_list(Path));

get_path_parts(Path) when erlang:is_atom(Path)->
    get_path_parts(erlang:atom_to_list(Path));

get_path_parts(Path) ->
    Parts = string:tokens(Path, "."),
    Func  = fun erlang:list_to_binary/1,

    lists:map(Func, Parts).

value_to_list(Value) when erlang:is_bitstring(Value) ->
    erlang:binary_to_list(Value);

value_to_list(Value) when erlang:is_bitstring(Value) ->
    erlang:integer_to_list(Value);

value_to_list(Value) ->
    Value.

type_of(Val) when is_integer(Val)   -> integer;
type_of(Val) when is_float(Val)     -> float;
type_of(Val) when is_list(Val)      -> list;
type_of(Val) when is_tuple(Val)     -> tuple;
type_of(Val) when is_bitstring(Val) -> bitstring;
type_of(Val) when is_binary(Val)    -> binary;
type_of(Val) when is_boolean(Val)   -> boolean;
type_of(Val) when is_function(Val)  -> function;
type_of(Val) when is_pid(Val)       -> pid;
type_of(Val) when is_port(Val)      -> port;
type_of(Val) when is_reference(Val) -> reference;
type_of(Val) when is_atom(Val)      -> atom;
type_of(_Val)                       -> unknown.

get_type(Value) when erlang:is_list(Value) ->
    case is_string(Value) of
        true -> string;
        _    -> list
    end;

get_type(Val) ->
    type_of(Val).

is_integer_binary(B) ->
    try
        _ = erlang:binary_to_integer(B),
        true
    catch error:badarg ->
        false
    end.

is_string([]) -> true;
is_string([H|T]) -> is_integer(H) andalso H>=0 andalso is_string(T);
is_string(_) -> false.