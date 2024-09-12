# Hurl, a simple command-line HTTP client

Hurl is a simple tool used to make HTTP requests (http/1.1 and h2) while
introspecting different information such as the chosen IP of the server, the
TLS configuration chosen between the server and the client as well as the fields
given by the server. It is possible to build a request to submit information via
json or in the form of a 'multipart-form'. It allows several types of display
and inflates content if gzip or zlib is used. You can perform a hexdump,
normalise the json received or simply display the content as it is. The aim is
to propose a tool that can be used to debug and interact with an HTTP server by
displaying all the essential information from all the protocols involved in the
communication.

Let's start by installing the software:
```shell
$ opam pin add https://github.com/robur-coop/hurl.git
```

The hurl software is now installed on your opam and ready to use:
```shell
$ hurl https://robur.coop
HTTP/1.1 200 OK

content-type: text/html; charset=utf-8
etag: ----------------------------------------
content-length: 1500

...
```

You can change the way the content of the response is displayed. You can
'hexdump' the content or (if the server informs you that it is json content)
normalise the content to json format:
```shell
$ hurl http://pie.dev/get
HTTP/1.1 200 OK

Content-Type: application/json
Content-Length: 444
Connection: close
Access-Control-Allow-Origin: *
Access-Control-Allow-Credentials: true

{
  "args": {},
  "headers": {
    "Accept-Encoding": "gzip",
    "Connection": "Keep-Alive",
    "Content-Length": "0",
    "Host": "pie.dev",
    "User-Agent": "hurl/test"
  },
  "origin": "x.x.x.x",
  "url": "http://pie.dev/get"
}
```

Information such as DNS (the chosen IP address) and TLS can also be displayed:
```shell
$ hurl https://robur.coop/ --print=dshb
A robur.coop:
  193.30.40.138

TLS 1.3 with AEAD AES128 GCM

content-type: text/html; charset=utf-8
etag: ----------------------------------------
content-length: 1500

...
```

There are a whole host of options available:
- use our OCaml implementation for DNS resolution instead of using your system's
  resolver
- change the http protocol used
- specify a certain version of tls
- specify a resolver like the one at uncensoreddns.org
- accept certain certificates (such as self-signed ones)
- modify the header sent to the server
- add parameters to the URL
- submit files (via multipart)
- submit json content

To do this, we advise you to use the `--help` option, which describes all these
options.
