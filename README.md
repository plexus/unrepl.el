# unrepl.el

An Emacs client for [unrepl](https://github.com/cgrand/unrepl).

*This is an alpha level proof of concept. Very rough around the edges. YMMV.*

## Usage


Start a socket REPL, e.g.

```
JAVA_OPTS="-Dclojure.server.unrepl={:address \"127.0.0.1\" :port 5555 :accept clojure.core.server/repl}" lein run -m clojure.main/main
```

or

```
lein run -m clojure.main -e "(clojure.core.server/start-server {:port 5555 :name (str 'server) :accept 'clojure.core.server/repl}) (.join (Thread/currentThread))"
```

In Emacs

``` emacs-lisp
(load "path/to/unrepl.el")
```

Now do `M-x unrepl-connect`. You'll get a prompt: `Socket REPL host/port: `.
Type for instance `5555` or `your.host.com:1234`. It should pop up a
`*unrepl-repl*` buffer.

In a Clojure buffer you can enable `unrepl-mode` to get eval support (`C-x C-e`).

You can look at the `*unrepl-output*` buffer to see the raw unrepl EDN messages.


## License

Copyright &copy; Arne Brasseur 2017.

Distributed under the [Mozilla Public License 2.0](https://www.mozilla.org/media/MPL/2.0/index.txt).
