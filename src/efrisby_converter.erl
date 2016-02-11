-module(efrisby_converter).

%% Callbacks
-export([
    encode_headers/1
]).

encode_headers(Headers) ->
    Function = fun(Val) -> value_to_list(Val) end,
    Result   = lists:keymap(Function, 2, lists:keymap(Function, 1, Headers)),

    Result.

value_to_list(Value) when erlang:is_binary(Value) ->
    erlang:binary_to_list(Value);

value_to_list(Value) when erlang:is_integer(Value) ->
    erlang:integer_to_list(Value);

value_to_list(Value) ->
    Value.
