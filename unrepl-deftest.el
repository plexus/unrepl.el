(defmacro unrepl-deftest (name &rest body)
  "Macro to emulate a unrepl session without actually connecting to an external process. NAME is the name of the test, passed down to ERT. Body is regular lisp code, but can contain four special kind of forms.

  Receive a mock message from unrepl.

      (receive! \"[unrepl/hello]\")

  Assert that certain text is present in the REPL buffer. This consumes input, consecutive calls start checking where the previous call ended.

     (assert-repl \"my.ns=> \")

  Send some code to the REPL. This has the effect of inserting it into the REPL buffer, and emulating pressing return

     (type! \"(+ 8 8)\")

  Assert that certain forms were sent from Emacs to unrepl.

     (assert-sent \"(+ 8 8)\n\")"
  (declare (indent defun))
  (let ((repl-assert-pos 1)
        (output-assert-pos 1))
    `(ert-deftest ,name ()
       (let* ((repl-buffer (generate-new-buffer "unrepl-test-repl"))
              (input-buffer (generate-new-buffer "unrepl-test-input"))
              (output-buffer (generate-new-buffer "unrepl-test-output"))
              (send-string-fn (lambda (s) (with-current-buffer output-buffer (insert s))))
              (connection (unrepl-make-connection 'repl-buffer repl-buffer
                                                  'output-buffer input-buffer
                                                  'send-string-fn send-string-fn)))
         (with-current-buffer input-buffer (setq unrepl-connection connection))
         (with-current-buffer repl-buffer
           (setq unrepl-connection connection)
           ,@(mapcar (lambda (pair)
                       (seq-let [sym string] pair
                         (case sym
                           ('receive! `(unrepl-handle-output nil ,string))
                           ('assert-repl (progn
                                           (let* ((start repl-assert-pos)
                                                  (end (+ start (length string))))
                                             (setq repl-assert-pos end)
                                             `(should (equal ,string (buffer-substring-no-properties ,start ,end))))))
                           ('assert-sent (progn
                                           (let* ((start output-assert-pos)
                                                  (end (+ start (length string))))
                                             (setq output-assert-pos end)
                                             `(should (equal ,string (with-current-buffer output-buffer (buffer-substring-no-properties ,start ,end)))))))
                           ('type! `(progn (insert ,(cadr pair))
                                           (unrepl-repl-return)))
                           (t pair))))
                     body))
         (kill-buffer input-buffer)
         (kill-buffer repl-buffer)))))

(provide 'unrepl-deftest)
