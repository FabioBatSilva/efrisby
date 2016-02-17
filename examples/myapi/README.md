= REST hello world example

To try this example, you need GNU `make` and `git` in your PATH.

To build and run the example, use the following command:

[source,bash]
$ make run

Then point your browser to http://localhost:8080

== Example output

Request JSON:

[source,bash]
----
$ curl -i -H "Content-type: application/json" http://localhost:8080 -d '{"rest": "Hello World!"}'
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:16:46 GMT
content-length: 24
content-type: application/json
vary: application/json

{"rest": "Hello World!"}
----
