(require 'unrepl-deftest)

(unrepl-deftest hello-send-receive-results
  (receive! "[:unrepl/hello {:session :test123, :actions {:exit (do-exit)}}]")
  (receive! "[:prompt {clojure.core/*ns* #unrepl/ns my.ns}]")
  (assert-repl "my.ns=> ")

  (type! "(+ 8 8)")
  (assert-repl "(+ 8 8)\n")
  (assert-sent "(+ 8 8)\n")

  (receive! "[:read {:from [1 1], :to [2 1], :offset 0, :len 8} 1]")
  (receive! "[:started-eval {:actions {:interrupt (do), :background (do)}} 1]")
  (receive! "[:eval 16 1]")
  (receive! "[:prompt {:file \"unrepl-session\", :line 2, :column 1, :offset 8, clojure.core/*ns* #unrepl/ns other.ns}]")

  (assert-repl "16\n")
  (assert-repl "other.ns=> ")

  ;; doesn't really belong in an acceptance test but just to demonstrate that we
  ;; can also check state in these style of tests

  (should (equal (unrepl-@history) '(((result . 16)
                                      (id . 1)
                                      (position . 0)
                                      (form . "(+ 8 8)\n")
                                      (handler . unrepl-repl-eval-callback))))))

(provide 'unrepl-acceptance-test)
