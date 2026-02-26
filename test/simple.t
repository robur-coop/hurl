  $ hurl.srv 127.0.0.1:8000 --pid srv.pid &
  $ ./waitfile.exe srv.pid
  $ hurl --field-filter='-Date' http://localhost:8000/get User-Agent:hurl/test
  HTTP/1.1 200 OK
  
  Content-Type: application/json
  Server: bin/%%VERSION%%
  Access-Control-Allow-Origin: *
  Access-Control-Allow-Credentials: true
  Content-Length: 159
  Connection: close
  
  {
    "args": {},
    "headers": {
      "User-Agent": "hurl/test",
      "host": "localhost",
      "connection": "close",
      "content-length": "0"
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
  Content-Length: 178
  Connection: close
  
  {
    "args": {
      "foo": "bar"
    },
    "headers": {
      "User-Agent": "hurl/test",
      "host": "localhost",
      "connection": "close",
      "content-length": "0"
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
  $ hurl http://localhost:8000/get foo==bar foo==bar User-Agent:hurl/test -p=b
  {
    "args": {
      "foo": "bar",
      "foo": "bar"
    },
    "headers": {
      "User-Agent": "hurl/test",
      "host": "localhost",
      "connection": "close",
      "content-length": "0"
    },
    "origin": "127.0.0.1",
    "url": "http://localhost/get?foo=bar&foo=bar"
  }
  $ hurl http://localhost:8000/robot.txt -p=b
  User-Agent: *
  Disallow: /deny
  $ hurl http://localhost:8000/get -p=b foo=bar -o file.json
  $ cat file.json
  {
    "args": {},
    "headers": {
      "Content-Type": "application/json",
      "user-agent": "hurl/0.2.0-2-g86c97a8",
      "host": "localhost",
      "transfer-encoding": "chunked"
    },
    "origin": "127.0.0.1",
    "url": "http://localhost/get"
  }
  $ kill -INT $(cat srv.pid)
