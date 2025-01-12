  $ hurl.srv 127.0.0.1:9000 --pid srv.pid &
  $ hurl --field-filter='-Date' http://localhost:9000/gzip User-Agent:hurl/test
  HTTP/1.1 200 OK
  
  Content-Type: application/json
  Server: bin/%%VERSION%%
  Access-Control-Allow-Origin: *
  Access-Control-Allow-Credentials: true
  Content-Length: 220
  Connection: close
  Content-Encoding: gzip
  
  {
    "gzipped": true,
    "args": {},
    "headers": {
      "Content-Type": "application/json",
      "User-Agent": "hurl/test",
      "host": "localhost",
      "transfer-encoding": "chunked"
    },
    "origin": "127.0.0.1",
    "url": "http://localhost/gzip"
  }
  $ hurl --field-filter='-Date' http://localhost:9000/deflate User-Agent:hurl/test
  HTTP/1.1 200 OK
  
  Content-Type: application/json
  Server: bin/%%VERSION%%
  Access-Control-Allow-Origin: *
  Access-Control-Allow-Credentials: true
  Content-Length: 176
  Connection: close
  Content-Encoding: deflate
  
  {
    "deflated": true,
    "args": {},
    "headers": {
      "Content-Type": "application/json",
      "User-Agent": "hurl/test",
      "host": "localhost",
      "transfer-encoding": "chunked"
    },
    "origin": "127.0.0.1",
    "url": "http://localhost/deflate"
  }
  $ kill -INT $(cat srv.pid)
