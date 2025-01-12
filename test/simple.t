  $ hurl.srv 127.0.0.1:8000 --pid srv.pid &
  $ hurl --field-filter='-Date' http://localhost:8000/get User-Agent:hurl/test
  HTTP/1.1 200 OK
  
  Content-Type: application/json
  Server: bin/%%VERSION%%
  Access-Control-Allow-Origin: *
  Access-Control-Allow-Credentials: true
  Content-Length: 181
  Connection: close
  
  {
    "args": {},
    "headers": {
      "Content-Type": "application/json",
      "User-Agent": "hurl/test",
      "host": "localhost",
      "transfer-encoding": "chunked"
    },
    "origin": "127.0.0.1",
    "url": "http://localhost/get"
  }
  $ hurl --field-filter='-Date' http://localhost:8000/get User-Agent:hurl/test foo==bar
  HTTP/1.1 200 OK
  
  Content-Type: application/json
  Server: bin/%%VERSION%%
  Access-Control-Allow-Origin: *
  Access-Control-Allow-Credentials: true
  Content-Length: 200
  Connection: close
  
  {
    "args": {
      "foo": "bar"
    },
    "headers": {
      "Content-Type": "application/json",
      "User-Agent": "hurl/test",
      "host": "localhost",
      "transfer-encoding": "chunked"
    },
    "origin": "127.0.0.1",
    "url": "http://localhost/get?foo=bar"
  }
  $ hurl --field-filter='-Date' http://localhost:8000/get?foo=bar
  [00][      WARN][         application]: the url path contains characters that have just been escaped. If you are trying to specify parameters in the url, you should do so via the command-line (rather than directly in the url).
  HTTP/1.1 404 Not Found
  
  Content-Type: text/html
  Server: bin/%%VERSION%%
  Access-Control-Allow-Origin: *
  Access-Control-Allow-Credentials: true
  Content-Length: 61
  Connection: close
  
  <!doctype html><title>404 Not found</title><h1>Not found</h1>
  $ hurl http://localhost:8000/get foo==bar foo==bar User-Agent:hurl/test --print=b
  {
    "args": {
      "foo": "bar",
      "foo": "bar"
    },
    "headers": {
      "Content-Type": "application/json",
      "User-Agent": "hurl/test",
      "host": "localhost",
      "transfer-encoding": "chunked"
    },
    "origin": "127.0.0.1",
    "url": "http://localhost/get?foo=bar&foo=bar"
  }
  $ hurl http://localhost:8000/robot.txt --print=b
  User-Agent: *
  Disallow: /deny
  
  $ kill -INT $(cat srv.pid)
