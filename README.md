# Wet Sprockets

## A WebSockets Library for Common Lisp

This is a WebSockets client[1] library for Common Lisp.

It should pass section "1 Framing" of the Autobahn WebSocket Testsuite.

Before running `bin/run-tests.sh` the Autobahn testsuite should be running:

- `docker run -it --rm -v "$PWD/config:/config" -v "$PWD/reports:/reports" -p 9001:9001 --name fuzzingserver crossbario/autobahn-testsuite`

[1] for now

### Known Issues

- only client mode is currently supported

### Resources

#### Specifications

- https://tools.ietf.org/html/rfc6455
- https://html.spec.whatwg.org/multipage/comms.html#network

#### Testing

- https://github.com/crossbario/autobahn-testsuite
    - https://stackoverflow.com/questions/33318976/websocket-stress-test-with-autobahn-testsuite
- https://libwebsockets.org/lws-api-doc-master/html/md_README.test-apps.html
- http://websocketstest.com/
- https://www.owasp.org/index.php/Testing_WebSockets_(OTG-CLIENT-010)
