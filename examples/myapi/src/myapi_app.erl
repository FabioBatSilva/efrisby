%% @private
-module(myapi_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", myapi_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(myapi, 100, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    myapi_sup:start_link().

stop(_State) ->
    ok.
