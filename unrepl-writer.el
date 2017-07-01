;;; unrepl-edn.el --- Clojure unrepl client              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the Mozilla Public License Version 2.0

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the Mozilla Public License along with this
;; program. If not, see <https://www.mozilla.org/media/MPL/2.0/index.txt>.

;;; Commentary:

;; A client for the unrepl protocol, for interactive Clojure development.
;; This file provides function for dealing with Clojure syntax (edn)

;;; Code:

(defun unrepl--elision? (x)
  (unrepl--tagged-literal? x 'unrepl/...))

(defun unrepl--insert-elision-button (edn)
  (insert-button
   "..."
   'buffer (current-buffer)
   'point (point)
   'get-more (gethash :get (third edn))
   'action (lambda (b)
             (save-excursion
               (with-current-buffer (button-get b 'buffer)
                 (unrepl-eval
                  (concat (edn-print-string (button-get b 'get-more)) "\n")
                  (lambda (result)
                    (let ((inhibit-read-only t))
                      (save-excursion
                        (with-current-buffer (button-get b 'buffer)
                          (goto-char (button-get b 'point))
                          (delete-char 3)
                          (unrepl--write-list-inner result)
                          ))))))))))

(defun unrepl--write-list-inner (edn)
  (-map-indexed (lambda (idx x)
                  (when (> idx 0)
                    (insert " "))
                  (if (unrepl--elision? x)
                      (unrepl--insert-elision-button x)
                    (unrepl--write x)))
                edn))

(defun unrepl--write-list (edn)
  (insert "(")
  (unrepl--write-list-inner edn)
  (insert ")"))

(defun unrepl--write-tagged-literal (edn)
  (let ((tag (second edn))
        (value (third edn)))
    (case tag
      ('unrepl/ratio (progn
                       (insert (number-to-string (elt value 0)))
                       (insert "/")
                       (insert (number-to-string (elt value 1)))))
      ('unrepl/pattern (progn
                         (insert "#")
                         (unrepl--write value)))
      ('unrepl/meta (progn
                      (insert "^")
                      (unrepl-write (elt value 0))
                      (insert " ")
                      (unrepl-write (elt value 1))))
      (t (progn
           (insert "#")
           (insert (symbol-name tag))
           (insert " ")
           (unrepl--write value))))))

(defun unrepl--write (edn)
  (cond
   ((eq edn nil)
    (insert "nil"))

   ((listp edn)
    (if (unrepl--tagged-literal? edn)
        (unrepl--write-tagged-literal edn)
      (unrepl--write-list edn)))

   (t
    (insert (edn-print-string edn)))))

(provide 'unrepl-writer)
