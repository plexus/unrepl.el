;;; unrepl.el --- Clojure unrepl client              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Keywords: tools, lisp, comm
;; Package-Requires: ((clojure-mode "") (dash "") (let-alist "") (peg "0.6") (edn ""))

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

(require 'unrepl)
(require 'ert)

(ert-deftest unrepl--insert-with-face-test ()
  (with-temp-buffer
    (unrepl--insert-with-face "test" 'font-lock-warning-face)
    (let ((str (buffer-string)))
      (should (equal str "test"))
      (should (equal (get-text-property 1 'face) 'font-lock-warning-face)))))
