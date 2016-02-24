# efrisby

[![Build Status](https://travis-ci.org/FabioBatSilva/efrisby.svg)](https://travis-ci.org/FabioBatSilva/efrisby)

A REST API testing framework for erlang inspired by frisby-js

## Installation

By adding the following dependency to your ```rebar.config``` file :

```erlang

%% Rebar3 test profiles
{profiles, [
    {test, [
        {deps, [
            {efrisby, "0.1.0"}
        ]}
    ]}
]}.

```

## Basic Usage.

```erlang
hackney:start().
%% > ok

efrisby:get("http://localhost/api/1.0/users/3.json", [
    {status, 200},
    {content_type, "application/json"},
    {json_types, [
        {<<"id">>, integer},
        {<<"is_admin">>, boolean},
        {<<"username">>, bitstring}
    ]},
    {json, [
        {<<"id">>, 3},
        {<<"is_admin">>, false},
        {<<"username">>, <<"johndoe">>}
    ]}
]).
%% > {ok, Response}

```


## Write Tests

efrisby tests start with one by calling one of ``get``, ``post``, ``put``, ``options``, ``delete``, or ``head`` to generate an HTTP request and assert the response.

efrisby has many built-in test assertions like :

* ``status`` to easily test HTTP status codes
* ``content_type`` to test content type header
* ``headers`` to test expected HTTP headers
* ``json`` to test expected JSON keys/values
* ``json_types`` to test JSON value types

eq :

```erlang

{ok, Response} = efrisby:get("https://api.github.com/users/FabioBatSilva", [
    {status, 200},
    {content_type, "application/json; charset=utf-8"},
    {json_types, [
        {<<"id">>, integer},
        {<<"url">>, bitstring},
        {<<"login">>, bitstring}
    ]},
    {json, [
        {<<"id">>, 588172},
        {<<"login">>, <<"FabioBatSilva">>},
        {<<"url">>, <<"https://api.github.com/users/FabioBatSilva">>}
    ]}
]).

> efrisby_resp:json(Response).
[
    {<<"id">>, 588172},
    {<<"login">>, <<"FabioBatSilva">>},
    {<<"url">>, <<"https://api.github.com/users/FabioBatSilva">>}
]
```

## Expectations

Expectation are used to generate assertions on the response.
These helpers make testing API response bodies and headers easy with minimal time and effort.


#### ``{status, integer()}``

Assert that HTTP Status code equals expectation.

```erlang
efrisby:get("https://api.github.com/users/FabioBatSilva", [
    {status, 200}
]).
%% > {ok, Response}
```


#### ``{headers, list()}``

Assert the HTTP response headers.

```erlang
efrisby:get("https://api.github.com/users/FabioBatSilva", [
    {headers, [
        {<<"content-type">>, <<"application/json; charset=utf-8">>}
    ]}
]).
%% > {ok, Response}
```


#### ``{json, bitstring() | none(), list() | integer() | atom() | bitstring()}``

Tests that response JSON body contains the provided keys/values in the response.

```erlang
efrisby:get("https://api.github.com/users/FabioBatSilva", [
    {json, [
        {<<"id">>, 588172},
        {<<"login">>, <<"FabioBatSilva">>}
    ]}
]).
%% > {ok, Response}

efrisby:get("https://api.github.com/users/FabioBatSilva", [
    {json, ".id", 588172},
    {json, ".login", <<"FabioBatSilva">>}
]).
%% > {ok, Response}
```


#### ``{json, bitstring() | none(), list()}``

Tests that response JSON body contains the provided keys/values types.

```erlang
efrisby:get("https://api.github.com/users/FabioBatSilva", [
    {json_types, [
        {<<"id">>, integer},
        {<<"login">>, bitstring}
    ]}
]).
%% > {ok, Response}
```

#### Using Paths with ``json`` and ``json_types``

Both ``json`` and ``json_types`` accept a tuple containing a path.
The path value can be a nested path separated by periods, like ``args.foo.mypath``, a simple path like ``.results`` or ``.`` to test the whole JSON value.

```erlang
efrisby:get("http://httpbin.org/get?foo=bar&bar=baz", [
    {json_types, ".args", [
        {<<"bar">>, bitstring},
        {<<"foo">>, bitstring}
    ]},
    {json, ".args", [
        {<<"foo">>, <<"bar">>},
        {<<"bar">>, <<"baz">>}
    ]}
]).
%% > {ok, Response}
```

## Request Methods

efrisby support your basic HTTP verbs..

* ``efrisby:get(url(), expectations(), options())``
* ``efrisby:head(url(), expectations(), options())``
* ``efrisby:options(url(), expectations(), options())``
* ``efrisby:put(url(), body(), expectations(), options())``
* ``efrisby:post(url(), body(), expectations(), options())``
* ``efrisby:patch(url(), body(), expectations(), options())``
* ``efrisby:delete(url(), body(), expectations(), options())``


Every request method accepts an optional list of parameters as its last argument,
Request options control various aspects of a request including, headers, timeout settings and more.

eq :

```erlang
-define(OPTIONS, [
    {failure_callback, fun erlang:display/1},
    {http_options, [{timeout, 150000}]},
    {base_url, "https://myapi.local"},
    {headers, [
        {"Accept", "application/json"}
    ]}
]).
```

* ``http_options`` Options for hackney http client (see https://github.com/benoitc/hackney)
* ``failure_callback`` Assertion failure callback function
* ``base_url`` Base request url
* ``headers`` Http headers

#### ``efrisby:get(url(), expectations(), options())``

GET request.

```erlang
efrisby:get("/users/FabioBatSilva", [
    {status, 200}
], ?OPTIONS).
%% > {ok, Response}
```

#### ``efrisby:post(url(), body(), expectations(), options())``

POST request.

```erlang

Body = [
    {<<"login">>, <<"FabioBatSilva">>},
    {<<"name">>, <<"Fabio B. Silva">>}
],

Expectations = [
    {status, 200},
    {content_type, "application/json"},
    {json_type, [
        {id, integer}
    ]}
],

efrisby:post("/users", Body, Expectations, ?OPTIONS).
%% > {ok, Response}
```