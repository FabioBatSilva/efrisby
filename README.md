# efrisby

[![Build Status](https://travis-ci.org/FabioBatSilva/efrisby.svg)](https://travis-ci.org/FabioBatSilva/efrisby)

A REST API testing framework for erlang inspired by frisby-js

## Documentation
DBD

## Installation

By adding the following dependency to your ```rebar.config``` file :

```erlang

%% Rebar deps
{deps, [
    {efrisby, ".*", {git, "git://github.com/FabioBatSilva/efrisby.git"}}
]}.

```

## Basic Usage.

```erlang
inets:start(),
%% > ok

efrisby:get("http://localhost/api/1.0/users/3.json", [
    {status, 200},
    {content_type, "application/json"},
    {json_types, [
        {<<"id">>, integer},
        {<<"is_admin">>, boolean},
        {<<"username">>, bitstring}
    ]},
    {json, {
        [
            {<<"id">>, 3},
            {<<"is_admin">>, false},
            {<<"username">>, <<"johndoe">>}
        ]
    }}
]).
%% > {ok, Response}

```
