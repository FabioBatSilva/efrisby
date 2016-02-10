-module(efrisby_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

simple_test() ->
    erlang:display(
        inets:start()
    ),
    erlang:display(
        httpc:request("http://httpbin.org/get")
    ),
    ?assertEqual(ok, efrisby:create()).