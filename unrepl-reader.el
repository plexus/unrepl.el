;;; unrepl-reader.el --- Clojure unrepl client

;; Copyright (C) 2017  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the Mozilla Public License Version 2.0

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the Mozilla Public License for more details.

;; You should have received a copy of the Mozilla Public License along with this
;; program. If not, see <https://www.mozilla.org/media/MPL/2.0/index.txt>.

;;; Commentary:

;; A client for the unrepl protocol, for interactive Clojure development

;;; Code:

(require 'peg)
(require 'edn)

(setq unrepl-tagged-literal-tag 'TAGGED1337)

(defun unrepl--tagged-literal? (val &rest args)
  (and (listp val)
       (eq (car val) unrepl-tagged-literal-tag)
       (or (not args)
           (eq (cadr val) (car args)))))


(defun unrepl--edn-read ()
  (let (discarded)
    (car
     (peg-parse
      (form _ (opt (or elide value err
                       )) _)

      (value (or string char bool integer float symbol keyword list vector map
                 set tagged-value quoted-form
                 ))

      (quoted-form  "'" value `(x -- (list 'quote x)))

      (char (substring "\\" (or "newline" "return" "space" "tab"
                                (and "u" alphanum alphanum alphanum alphanum)
                                alphanum))
            (if terminating)
            `(c -- (edn--create-char c)))

      (bool (substring (or "true" "false"))
            `(bool -- (when (string-equal bool "true") t)))

      (symbol (substring (or slash symbol-with-prefix symbol-no-ns))
              (if terminating) `(symbol -- (intern symbol)))
      (additional-symbol-chars ["*+!-_?$%&=<>:#."])
      (symbol-constituent (or alphanum additional-symbol-chars))
      (symbol-start (or alpha ["*!_?$%&=<>"]
                        (and (or "-" "+" ".") (not integer1)
                             (* (or alpha additional-symbol-chars)))))
      (slash "/")
      (symbol-with-prefix symbol-start (* symbol-constituent) slash
                          (+ symbol-constituent))
      (symbol-no-ns symbol-start (* symbol-constituent))

      (keyword-with-prefix keyword-start (* symbol-constituent) slash
                           (or (and (or symbol-start "#")
                                    (* symbol-constituent))
                               (and ":" (+ symbol-constituent))))

      (keyword-no-ns keyword-start (opt (+ symbol-constituent)))

      (keyword (substring (or keyword-with-prefix keyword-no-ns))
               (if terminating) `(kw -- (intern kw)))
      (keyword-start ":" (or alphanum ["*+!-_?$%&=<>#."]))

      (string "\"" (substring string-content) "\""
              `(str -- (edn--create-string str)))
      (string-content (* (or "\\" (not "\"")) (any)))
      (string1 "\"" string-content "\"")

      (integer (substring integer1) (if terminating)
               `(i -- (string-to-number i)))
      (integer1 (or "+" "-" "")
                (or (and [1-9] (* [0-9]))
                    [0-9]))

      (float (substring float1) (if terminating)
             `(f -- (string-to-number f)))

      (float1 (or (and integer1 frac exp)
                  (and integer1 frac)
                  (and integer1 exp)))

      (list "(" `(-- nil)
            (* _ (or elide value) _ `(-- (edn--maybe-add-to-list)) `(e _ -- e))
            ")" `(l -- (nreverse l)))

      (vector "[" `(-- nil)
              (* _ (or elide value) `(-- (edn--maybe-add-to-list)) `(e _ -- e))
              _ "]" `(l -- (vconcat (nreverse l))))

      (map "{" `(-- nil)
           (* _ (or elide value) `(-- (edn--maybe-add-to-list)) `(e _ -- e))
           _ "}" `(l -- (edn--create-hash-table (nreverse l))))

      (set "#{" `(-- nil)
           (* _ (or elide value) `(-- (edn--maybe-add-to-list)) `(e _ -- e))
           _ "}" `(l -- (edn-list-to-set (nreverse l))))

      (tagged-value "#" (substring alpha (or (and (* symbol-constituent) slash
                                                  (+ symbol-constituent))
                                             (* symbol-constituent)))
                    _ value _ `(tag val -- (list unrepl-tagged-literal-tag (intern tag) val))
                    )

      (frac "." (+ digit))
      (exp ex (+ digit))
      (ex (or "e" "E") (opt (or "-" "+")))

      (digit [0-9])
      (upper [A-Z])
      (lower [a-z])
      (alpha (or lower upper))
      (alphanum (or alpha digit))
      (terminating (or (set " \n\t()[]{}\";,") (eob)))
      (_ (* (or ws comment)))
      (comment ";" (* (not (or "\n" (eob))) (any)))
      (elide "#_" _ value `(-- (setq discarded t)) `(e _ _ -- e))
      (ws ["\t \n,"])

      (unsupported-bignum (substring (or float1 integer1) (or "N" "M"))
                          terminating
                          `(n -- (error "Unsupported bignum: %s" n)))
      (err (or unsupported-bignum (substring (+ (any))))
           `(s -- (error "Invalid edn: '%s'" s) nil))))))

(provide 'unrepl-reader)
