# Wet Sprockets

The canonical home page of this library is
https://git.sr.ht/~aerique/wet-sprockets

(The library is also pushed to GitLab and GitHub but those sites are not
monitored for support.)

## A WebSockets Library for Common Lisp

This is a WebSockets client library for Common Lisp.

It should pass sections 1, 2, 3 and 4 of the Autobahn WebSocket Testsuite.

Before running `bin/run-tests.sh` the Autobahn testsuite should be running:

- `docker run -it --rm -v "$PWD/config:/config" -v "$PWD/reports:/reports" -p 9001:9001 --name fuzzingserver crossbario/autobahn-testsuite`

## Notes on Tests

Some Autobahn tests are harder to properly test.  For example: 2.10,
2.11, 3.1, 3.2, 3.3, 5.9 to 5.20 (more or less).

We're testing at a pretty low level and these tests would need a proper
event loop running.  We could start a listener since it has to handle
these cases, but I'm not quite sure about the proper approach yet.

For now, these tests look kinda stupid.

### Known Issues

- fragmenting support is clunky
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
