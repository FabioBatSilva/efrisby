-module(efrisby_json_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

get_root_data_test() ->
    Json = {
        [
            {
                <<"args">>, {
                    [{<<"foo">>,<<"bar">>}]
                }
            }
        ]
    },

    ?assertEqual(Json, efrisby_json:get_data(".", Json)),
    ?assertEqual(Json, efrisby_json:get_data(<<".">>, Json)).

get_data_from_path_test() ->
    Args = {
        [
            {<<"foo">>,<<"bar">>}
        ]
    },
    Json = {
        [
            {<<"args">>, Args}
        ]
    },

    ?assertEqual(Args, efrisby_json:get_data(".args", Json)).

get_data_from_complex_path_test() ->
    Json = {
        [
            {
                <<"headers">>,
                {
                    [
                        {<<"Host">>, <<"httpbin.org">>}
                    ]
                }
            }
        ]
    },

    ?assertEqual(<<"httpbin.org">>, efrisby_json:get_data(".headers.Host", Json)),
    ?assertEqual(<<"httpbin.org">>, efrisby_json:get_data(<<".headers.Host">>, Json)).


get_data_from_json_binary_test() ->
    Body     = <<"{\"args\":{\"foo\":\"bar\"}}">>,
    Actual   = efrisby_json:get(".args", Body),
    Expected = {
        [
            {<<"foo">>,<<"bar">>}
        ]
    },

    ?assertEqual(Expected, Actual).

get_data_from_json_string_test() ->
    Body     = "{\"args\":{\"foo\":\"bar\"}}",
    Actual   = efrisby_json:get(".args", Body),
    Expected = {
        [
            {<<"foo">>,<<"bar">>}
        ]
    },

    ?assertEqual(Expected, Actual).