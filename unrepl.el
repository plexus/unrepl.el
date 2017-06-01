;;; unrepl.el --- Clojure unrepl client              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Arne Brasseur

;; Author: Arne Brasseur <arne@bmo>
;; Keywords: tools, lisp, comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unrepl-read-blob ()
  ;;TODO don't hard code this
  (get-string-from-file "/home/arne/github/unrepl.el/blob.clj"))

(defvar-local unrepl-repl-input-start-mark nil)
(defvar-local unrepl-hello-payload nil)

(edn-add-reader :unrepl/param (lambda (x) `(:unrepl/param . ,x)))
(edn-add-reader :unrepl/ns (lambda (x) `(:unrepl/ns . ,x)))
(edn-add-reader :unrepl/ratio (lambda (x) `(:unrepl/ratio . ,x)))
(edn-add-reader :unrepl/meta (lambda (x) `(:unrepl/meta . ,x)))
(edn-add-reader :unrepl/pattern (lambda (x) `(:unrepl/pattern . ,x)))
(edn-add-reader :unrepl/object (lambda (x) `(:unrepl/object . ,x)))
(edn-add-reader :unrepl.java/class (lambda (x) `(:unrepl.java/class . ,x)))
(edn-add-reader :unrepl/... (lambda (x) `(:unrepl/... . ,x)))
(edn-add-reader :error #'identity)

(defun unrepl--insert-with-face (str face)
  (put-text-property 0 (length str) 'face face str)
  (insert str))

(defun unrepl--handle-hello (payload)
  (setq unrepl-hello-payload payload))

(defun unrepl--handle-prompt (payload)
  (when (not (eq (current-column) 0))
    (unrepl--insert-with-face "%" 'custom-set)
    (insert "\n"))
  (unrepl--insert-prompt
   (symbol-name
    (cdr (gethash 'clojure.core/*ns* payload)))))

;; TODO store the payload so we can :interrupt or :background
(defun unrepl--handle-started-eval (payload))

(defun unrepl--handle-out (payload)
  (if (eq (current-column) 0)
      (unrepl--insert-with-face "#_out| " 'font-lock-constant-face))
  (insert payload))

(defun unrepl--handle-err (payload)
  (if (eq (current-column) 0)
      (unrepl--insert-with-face "#_err| " 'font-lock-warning-face))
  (insert payload))

(defun unrepl--handle-eval (payload)
  (when (not (eq (current-column) 0))
    (unrepl--insert-with-face "%" 'custom-set)
    (insert "\n"))
  (insert (edn-print-string payload))
  (insert "\n"))

(defun unrepl--handle-bye (payload))

(defun unrepl--handle-log (payload))

(defun unrepl--handle-exception (payload)
  (if (not (eq (current-column) 0))
      (insert "\n"))
  (unrepl--insert-with-face "#_exception| " 'font-lock-warning-face)
  (insert (gethash :cause (gethash :ex payload))))

(defun unrepl-edn-handler (form)
  ;;(message (concat "->" (prin1-to-string form)))
  (with-current-buffer (get-buffer-create "*unrepl-repl*")
    (let ((tag (elt form 0))
          (payload (elt form 1)))
      (case tag
        (:unrepl/hello (unrepl--handle-hello payload))
        (:prompt (unrepl--handle-prompt payload))
        (:started-eval (unrepl--handle-started-eval payload))
        (:eval (unrepl--handle-eval payload))
        (:out (unrepl--handle-out payload))
        (:err (unrepl--handle-err payload))
        (:bye (unrepl--handle-bye payload))
        (:log (unrepl--handle-log payload))
        (:exception (unrepl--handle-exception payload))))))

(defun unrepl-handle-output (proc string)
  (with-current-buffer (get-buffer-create "*unrepl-output*")
    (let ((orig-point (point)))
      (insert string)
      (goto-char orig-point)
      ;;(message (prin1-to-string (list :ok orig-point (eq orig-point 0) string)))

      (when (eq orig-point 1)
        (right-char 7)
        (process-send-string unrepl-process (unrepl-read-blob))
        ;; (search-forward "[:unrepl/hello")
        ;; (left-char (length "[:unrepl/hello"))
        )

      (let ((form (edn-read)))
        (while form
          (unrepl-edn-handler form)
          (setq form (edn-read)))))))


(defun unrepl-process-sentinel (proc event)
  ;;(message (concat "EVT->" (prin1-to-string event)))
  )

(defun unrepl--insert-prompt (namespace)
  (goto-char (point-max))
  (if (not (eq (current-column) 0))
      (insert "\n"))
  (let ((prompt-start (point)))
    (insert (concat namespace "=> "))
    (setq unrepl-repl-input-start-mark (point))
    (add-text-properties prompt-start (point)
                         '(font-lock-face font-lock-keyword-face
                           read-only t
                           intangible t
                           ;; field cider-repl-prompt
                           rear-nonsticky (field read-only font-lock-face intangible)))))


(defun unrepl-repl-return ()
  (interactive)
  (insert "\n")
  (condition-case nil
      (let ((cmd (buffer-substring-no-properties unrepl-repl-input-start-mark (point-max))))
        (edn-read cmd)
        (process-send-string unrepl-process cmd))
    (error (insert "#_=> "))))

(defun unrepl-start (command)
  (when (get-buffer "*unrepl-output*")
    (kill-buffer "*unrepl-output*"))

  (with-current-buffer (get-buffer-create "*unrepl-repl*")
    (unrepl-repl-mode))

  (setq unrepl-process (make-process
                        :name "unrepl"
                        :buffer "*unrepl-process*"
                        :command command
                        :filter #'unrepl-handle-output
                        :sentinel #'unrepl-process-sentinel
                        :connection-type 'pipe)))

(defun unrepl-connect (host-port)
  (interactive "MSocket repl host/port: ")

  (when (get-buffer "*unrepl-output*")
    (kill-buffer "*unrepl-output*"))

  (with-current-buffer (get-buffer-create "*unrepl-repl*")
    (unrepl-repl-mode))

  (let* ((host-port (s-split ":" host-port))
         (host-given? (= (length host-port) 2))
         (host (if host-given?
                   (first host-port)
                 "localhost"))
         (port (string-to-number
                (if host-given?
                    (second host-port)
                  (first host-port)))))
    (setq unrepl-process (make-network-process
                          :name "unrepl"
                          :buffer "*unrepl-process*"
                          :host host
                          :service port
                          :filter #'unrepl-handle-output
                          :sentinel #'unrepl-process-sentinel))
    (switch-to-buffer "*unrepl-repl*")))

(defun unrepl-eval-last-sexp ()
  (interactive)
  (process-send-string unrepl-process (concat (cider-last-sexp) "\n")))

(defun unrepl-close ()
  (interactive)
  (when (get-buffer "*unrepl-output*")
    (kill-buffer "*unrepl-output*"))
  (when (get-buffer "*unrepl-repl*")
    (kill-buffer "*unrepl-repl*"))
  (when (get-buffer "*unrepl-process*")
    (kill-buffer "*unrepl-process*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar unrepl-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'unrepl-repl-return)
    map))

(define-derived-mode unrepl-repl-mode fundamental-mode "Unrepl")

(defvar unrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'unrepl-eval-last-sexp)
    map))

(define-minor-mode unrepl-mode
  nil
  "unrepl"
  unrepl-mode-map)

;; (progn
;;   (when (get-buffer "*unrepl-process*")
;;     (kill-buffer "*unrepl-process*"))
;;   (when (get-buffer "*unrepl-repl*")
;;     (kill-buffer "*unrepl-repl*"))
;;   (unrepl-start '("telnet" "localhost" "3848")))

;; to be able to start/stop repls quickly for testing, I start unrepl on a socket repl server, then connect to it with telnet
;;
;; JAVA_OPTS="-Dclojure.server.unrepl={:address \"127.0.0.1\"  :port 3848 :accept unrepl.repl/start}" lein run -m clojure.main/main

(provide 'unrepl)
;;; unrepl.el ends here
