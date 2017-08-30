# unrepl.el

[![Build Status](https://travis-ci.org/plexus/unrepl.el.svg?branch=master)](https://travis-ci.org/plexus/unrepl.el)

An Emacs client for [unrepl](https://github.com/cgrand/unrepl).

Depends on Emacs >= 25.

## Project status

*This is an alpha level proof of concept. Very rough around the edges. YMMV.*

So far this has been a proof of concept of writing an unrepl client for Emacs. It kind of works as far as giving you a basic REPL goes. From there it has spun into a load of yak shaving, working on [a.el](https://github.com/plexus/a.el), which is more or less finished and available in MELPA, and working on [parseclj](https://github.com/lambdaisland/parseclj/), which is still a work in progress.

All of this work is currently stalled as I had to take a step back from open source to deal with life and other things. I am committed to finishing parseclj to be a full-featured Clojure/EDN parser for Emacs Lisp, although I'm not making any claims about how long that will take. After that I may or may not revisit unrepl.el, which at that stage will more or less be a rewrite from scratch.

## Usage


Start a socket REPL, e.g.

```
JAVA_OPTS="-Dclojure.server.unrepl={:address \"127.0.0.1\" :port 5555 :accept clojure.core.server/repl}" lein run -m clojure.main/main
```

or with Leiningen

```
lein run -m clojure.main -e "(clojure.core.server/start-server {:port 5555 :name (str 'server) :accept 'clojure.core.server/repl}) (.join (Thread/currentThread))"
```

or with Boot >= 2.7.2

```
boot socket-server --port 5555 wait
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
