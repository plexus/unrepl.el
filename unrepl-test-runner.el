;; Script used for the Travis build

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ;;("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ))


(package-initialize)
(package-refresh-contents)

(package-install 'package-lint)
(package-install 'clojure-mode)
(package-install 'dash)
(package-install 'peg)
(package-install 'edn)

(let ((files '("unrepl-reader.el" "unrepl-writer.el" "unrepl.el" "unrepl-test.el"))
      (pwd (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "pwd"))))

  (add-to-list 'load-path pwd)

  (dolist (file files)
    (load (concat pwd "/" file)))

  (if (getenv "CLJ_PARSE_LINT")
      (let ((success t))
        (dolist (file files)
          (with-temp-buffer
            (insert-file-contents file t)
            (emacs-lisp-mode)
            (let ((checking-result (package-lint-buffer)))
              (when checking-result
                (setq success nil)
                (message "In `%s':" file)
                (pcase-dolist (`(,line ,col ,type ,message) checking-result)
                  (message "  at %d:%d: %s: %s" line col type message))))))
        (kill-emacs (if success 0 1)))
    (ert-run-tests-batch-and-exit)))
