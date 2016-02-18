##### REST efrisby example

To try this example, you need GNU `make`, `git` and `rebar3` in your PATH.

To build and run the example, use the following command:

```bash
$ make release start
```

Then point your browser to http://localhost:8080


To run the tests, use the following command:

```bash
$ make test
```

### Example output

Request JSON:

```bash
$ curl -i -XPOST -H "Content-type: application/json" "http://localhost:8080" -d '{"rest": "Hello World!"}'
HTTP/1.1 200 OK
server: Cowboy
date: Thu, 18 Feb 2016 00:11:54 GMT
content-length: 197
content-type: application/json

{
    "url": "http://localhost:8080/",
    "json": {
        "rest": "Hello World!"
    },
    "headers": {
        "host": "localhost:8080",
        "user-agent": "curl/7.43.0",
        "accept": "*/*",
        "content-type": "application/json",
        "content-length": "24"
    }
}

```
