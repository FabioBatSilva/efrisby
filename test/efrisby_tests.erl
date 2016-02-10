-module(efrisby_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

httpbin_get_request_test() ->
    erlang:display(
        inets:start()
    ),
    ?assertEqual(ok, efrisby:get("http://httpbin.org/get?foo=bar", [
        {status, 200},
        {content_type, "application/json"},
        {json, ".args", {
            [
                {<<"foo">>,<<"bar">>}
            ]
        }}
    ])).