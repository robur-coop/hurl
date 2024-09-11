# Hurl, a simple command-line HTTP client

Hurl is a simple tool used to make HTTP requests (http/1.1 and h2) while
introspecting different information such as the chosen IP of the server, the
TLS configuration chosen between the server and the client as well as the fields
given by the server.

It allows several types of display and inflates content if gzip or deflate is
used. You can perform a hexdump, normalise the json received or simply display
the content as it is.
