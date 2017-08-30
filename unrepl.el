;;; unrepl.el --- Clojure unrepl client              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Keywords: tools, lisp, comm
;; Package-Requires: ((clojure-mode "") (dash "") (let-alist "") (peg "0.6") (edn "") (a "0.1.0alpha4")))

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

(require 'let-alist)
(require 'clojure-mode)
(require 'dash)
(require 'ansi-color)
(require 'a)

(require 'unrepl-reader)
(require 'unrepl-writer)

(defvar unrepl-blob-path
  (expand-file-name "blob.clj" (file-name-directory load-file-name))
  "Path to the unrepl 'upgrade blob', created with `lein unrepl-make-blob'.")

(defvar-local unrepl-buffer-type nil)

(defvar unrepl-connections nil
  "List of unrepl connection objects")

(defvar-local unrepl-connection nil
  "Connection used by the current buffer. An alist of process, repl-buffer, output-buffer, position, history.")

(eval-when-compile
  (defmacro unrepl-deffield (scope name)
    "Creates getters and setters"
    `(progn
       (defun ,(intern (concat "unrepl-@" (symbol-name name))) ()
         (cdr (assq ',name ,scope)))
       (defun ,(intern (concat "unrepl--" (symbol-name name))) ()
         (assq ',name ,scope))
       (cl-defmethod (setf ,(intern (concat "unrepl-@" (symbol-name name)))) (val)
         (setf (cdr (assq ',name ,scope))
               val))))

  (defmacro unrepl-swap! (loc fn &rest args)
    `(setf ,loc (,fn ,loc ,@args))))

(unrepl-deffield (unrepl--connection) got-hello)
(unrepl-deffield (unrepl--connection) session)
(unrepl-deffield (unrepl--connection) actions)
(unrepl-deffield (unrepl--connection) process)
(unrepl-deffield (unrepl--connection) repl-buffer)
(unrepl-deffield (unrepl--connection) output-buffer)
(unrepl-deffield (unrepl--connection) position)
(unrepl-deffield (unrepl--connection) history)
(unrepl-deffield (unrepl--connection) write-fn)
(unrepl-deffield (unrepl--connection) prompt-end)
(unrepl-deffield (unrepl--connection) input-start)

(defmacro unrepl-with-readonly-insert (&rest body)
  `(let ((__p (point))
         (inhibit-read-only t))
     ,@body
     (add-text-properties __p (point) '(read-only t front-sticky t rear-nonsticky t))))

;; borrowed from CIDER
(defun unrepl-defun-at-point (&optional bounds)
  "Return the text of the top level sexp at point.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (clojure-backward-logical-sexp 1)
        (funcall (if bounds #'list #'buffer-substring-no-properties)
                 (point) end)))))

;; borrowed from CIDER
(defun unrepl-last-sexp (&optional bounds)
  "Return the sexp preceding the point.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
  (apply (if bounds #'list #'buffer-substring-no-properties)
         (save-excursion
           (clojure-backward-logical-sexp 1)
           (list (point)
                 (progn (clojure-forward-logical-sexp 1)
                        (skip-chars-forward "[:blank:]")
                        (when (looking-at-p "\n") (forward-char 1))
                        (point))))))

(defun unrepl--read-blob ()
  (with-temp-buffer
    (insert-file-contents unrepl-blob-path)
    (buffer-string)))

(defun unrepl--insert-with-face (str face)
  (put-text-property 0 (length str) 'face face str)
  (insert str))

(defun unrepl--history-search (history position index)
  (let ((top (car history)))
    (when top
      (if (<= (alist-get 'position top) position)
          index
        (unrepl--history-search (cdr history) position (1+ index))))))

(defun unrepl--history-by-position (position)
  (seq-elt (unrepl-@history)
           (unrepl--history-search (unrepl-@history) position 0)))

(cl-defmethod (setf unrepl--history-by-position) (value position)
  (setf (seq-elt
         (unrepl-@history)
         (unrepl--history-search (unrepl-@history) position 0))
        value))

(defun unrepl--history-idx (key value)
  (seq-position (unrepl-@history) value
                (lambda (x y)
                  (eq (cdr (assq key x)) y))))

(defun unrepl--history-by (key value)
  (seq-elt (unrepl-@history)
           (unrepl--history-idx key value)))

(cl-defmethod (setf unrepl--history-by) (cmd key value)
  (setf (seq-elt (unrepl-@history)
                 (unrepl--history-idx key value))
        cmd))

(defun unrepl--handle-hello (payload)
  ;; TODO: store the actions for later use
  ;;(setq unrepl-hello-payload payload)
  )

(defun unrepl--handle-prompt (payload)
  (when (not (eq (current-column) 0))
    (unrepl--insert-with-face "%" 'custom-set)
    (insert "\n"))
  (unrepl--insert-prompt
   (symbol-name
    (third (gethash 'clojure.core/*ns* payload)))))

(defun unrepl--handle-out (payload)
  (if (eq (current-column) 0)
      (unrepl--insert-with-face "#_out| " 'font-lock-constant-face))
  (insert (ansi-color-apply payload)))

(defun unrepl--handle-err (payload)
  (let ((line-prefix (buffer-substring-no-properties
                      (line-beginning-position)
                      (min (point-max) (+ 6 (line-beginning-position))))))
    (if (eq (current-column) 0)
        (unrepl--insert-with-face "#_err| " 'font-lock-warning-face)
      (when (not (equal "#_err|" line-prefix))
        (unrepl--insert-with-face "%" 'custom-set)
        (insert "\n")
        (unrepl--insert-with-face "#_err| " 'font-lock-warning-face))))
  (insert (ansi-color-apply payload)))

;; [:read {:from [2 1], :to [3 1], :offset 8, :len 8} 2]
(defun unrepl--handle-read (form)
  (seq-let [_ pos-info id] form
    (let* ((offset (gethash :offset pos-info)))
      (push (cons 'id id) (unrepl--history-by-position offset)))))

;; [:started-eval {:actions {:interrupt (unrepl.replG__1801/interrupt! :session2160 2), :background (unrepl.replG__1801/background! :session2160 2)}} 2]
(defun unrepl--handle-started-eval (payload))

;; [:eval #_result 14 #_id 2]
(defun unrepl--handle-eval (form)
  (seq-let [_ result id] form
    (push (cons 'result result) (unrepl--history-by 'id id))
    (funcall (cdr (assq 'handler (unrepl--history-by 'id id))) result))

  ;; (when (not (eq (current-column) 0))
  ;;   ;; taken out because when evaluating forms from the repl it would show up
  ;;   ;; for no good reason
  ;;   ;; (unrepl--insert-with-face "%" 'custom-set)
  ;;   (insert "\n"))
  ;;(unrepl--write payload)
  ;;(insert "\n")
  )

(defun unrepl--handle-bye (payload))

;; [:log [:info "user" #inst "2017-04-04T14:56:56.574-00:00" "a" (0 1 2 3 4 5 6 7 8 9 #unrepl/... {:get (unrepl.repl/fetch :G__3948)})] 12]
;; level, key, inst, args
(defun unrepl--handle-log (payload)
  (unrepl-with-readonly-insert
   (unrepl--insert-with-face "#_log| " 'font-lock-constant-face)
   (unrepl--write payload)))

(defun unrepl--handle-exception (payload)
  (unrepl-with-readonly-insert
   (if (not (eq (current-column) 0))
       (insert "\n"))
   (unrepl--insert-with-face "#_exception| " 'font-lock-warning-face)
   (insert (gethash :cause (caddr (gethash :ex payload))))
   (insert "\n")))

(defun unrepl-edn-handler (form)
  (with-current-buffer (unrepl-@repl-buffer)
    (let ((tag (elt form 0))
          (payload (elt form 1)))
      (case tag
        (:unrepl/hello (unrepl--handle-hello payload))
        (:prompt (unrepl--handle-prompt payload))
        (:read (unrepl--handle-read form))
        (:started-eval (unrepl--handle-started-eval form))
        (:eval (unrepl--handle-eval form))
        (:out (unrepl--handle-out payload))
        (:err (unrepl--handle-err payload))
        (:bye (unrepl--handle-bye payload))
        (:log (unrepl--handle-log payload))
        (:exception (unrepl--handle-exception payload))))))

(defun unrepl-handle-output (proc string)
  (with-current-buffer (unrepl-@output-buffer)
    (goto-char (point-max))
    (save-excursion (insert string))

    (let ((hello "[:unrepl/hello"))
      (when (string-match-p (regexp-quote hello) string)
        (search-forward hello)
        (left-char (length hello))
        (setf (unrepl-@got-hello) t)))

    (when (unrepl-@got-hello)
      (let ((form (unrepl--edn-read)))
        (while form
          (unrepl-edn-handler form)
          (setq form (unrepl--edn-read)))))))

(defun unrepl-process-sentinel (proc event)
  ;;(message (concat "EVT->" (prin1-to-string event)))
  )

(defun unrepl--insert-prompt (namespace)
  (goto-char (point-max))
  (if (not (eq (current-column) 0))
      (insert "\n"))
  (let ((prompt-start (point)))
    (insert (concat namespace "=> "))
    (setf (unrepl-@input-start) (point))
    (setf (unrepl-@prompt-end) (point))
    (add-text-properties prompt-start (point)
                         '(font-lock-face font-lock-keyword-face
                           read-only t
                           intangible t
                           front-sticky t
                           rear-nonsticky t))))


(defun unrepl--connection ()
  ;; TODO make this smarter, so buffers find the most suitable connection based
  ;; on path/project
  (or unrepl-connection (first unrepl-connections)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands and Modes

(defun unrepl-repl-eval-callback (result)
  (unrepl-with-readonly-insert
   (unrepl--write result)
   (insert "\n")))

(defun unrepl-repl-return ()
  (interactive)
  (when (eq (point) (line-end-position))
    (insert "\n")
    (let ((inhibit-read-only t))
      (add-text-properties (unrepl-@prompt-end) (point-max)
                           '(read-only t
                                       front-sticky t
                                       rear-nonsticky t)))
    (let ((cmd (buffer-substring-no-properties (unrepl-@input-start) (point-max))))
      (condition-case nil
          (scan-sexps (unrepl-@prompt-end) most-positive-fixnum)
        (error
         (insert "#_=> ")
         (add-text-properties (- (point) 5) (point)
                              '(font-lock-face font-lock-keyword-face
                                               read-only t
                                               intangible t
                                               front-sticky t
                                               rear-nonsticky t))))
      (setf (unrepl-@input-start) (point))
      (unrepl-eval cmd #'unrepl-repl-eval-callback))))

(defun unrepl-eval (form handler)
  (push (a-list 'position (unrepl-@position)
                'form form
                'handler handler)
        (unrepl-@history))
  (unrepl-send-string form)
  (unrepl-swap! (unrepl-@position) + (length form)))

(defun unrepl-make-connection (&rest kvs)
  (a-merge
   (a-list 'got-hello nil
           'session nil
           'actions nil
           'history nil
           'position 0
           'input-start 0
           'prompt-end 0)
   (apply 'a-list kvs)))

(defun unrepl-send-string (s)
  (funcall (a-get (unrepl--connection) 'send-string-fn) s))

(defun unrepl-connect (host-port)
  (interactive "MSocket repl host/port: ")

  (let* ((host-port (s-split ":" host-port))
         (host-given? (= (length host-port) 2))
         (host (if host-given?
                   (first host-port)
                 "localhost"))
         (port (string-to-number
                (if host-given?
                    (second host-port)
                  (first host-port))))
         (output-buffer-name (if unrepl-connections
                                 (concat "*unrepl-output<" (number-to-string (1+ (length unrepl-connections))) ">*")
                               "*unrepl-output*"))
         (repl-buffer-name (if unrepl-connections
                               (concat "*unrepl-repl<" (number-to-string (1+ (length unrepl-connections))) ">*")
                             "*unrepl-repl*")))

    (when (get-buffer output-buffer-name)
      (kill-buffer output-buffer-name))

    (let* ((process (make-network-process
                     :name "unrepl"
                     :buffer output-buffer-name
                     :host host
                     :service port
                     :filter #'unrepl-handle-output
                     :sentinel #'unrepl-process-sentinel))
           (repl-buffer (get-buffer-create repl-buffer-name))
           (output-buffer (get-buffer output-buffer-name))
           (connection (unrepl-make-connection
                        'process process
                        'repl-buffer repl-buffer
                        'output-buffer output-buffer
                        'send-string-fn (lambda (s) (process-send-string process s)))))

      (with-current-buffer output-buffer
        (setq unrepl-connection connection)
        (setq unrepl-buffer-type 'process))

      (with-current-buffer repl-buffer
        (unrepl-repl-mode)
        (setq unrepl-connection connection)
        (setq unrepl-buffer-type 'repl))

      (push connection unrepl-connections)
      (process-send-string process (unrepl--read-blob))
      (switch-to-buffer repl-buffer))))

(defun unrepl-eval-last-sexp ()
  (interactive)
  (unrepl-send-string (concat (unrepl-last-sexp) "\n")))

(defun unrepl-eval-defun ()
  (interactive)
  (unrepl-send-string (concat (unrepl-defun-at-point) "\n")))

(defun unrepl-close-all ()
  (interactive)
  (-map (lambda (conn)
          (let-alist conn
            (kill-buffer .repl-buffer)
            (kill-buffer .output-buffer)))
        unrepl-connections)
  (setq unrepl-connections nil))

(defvar unrepl-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'unrepl-repl-return)
    map))

(define-derived-mode unrepl-repl-mode fundamental-mode "Unrepl")

(defvar unrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'unrepl-eval-last-sexp)
    map))

;;;###autoload
(define-minor-mode unrepl-mode
  "Minor mode to get evaluation capabilities in source buffers."
  nil
  " UN"
  unrepl-mode-map)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(defun unrepl--kill-buffer-hook-handler ()
  "Clean up when closing either the repl or the output/process
buffer. Close the other buffer, and remove the connection from
the list."
  (when (or (eq unrepl-buffer-type 'repl) (eq unrepl-buffer-type 'process))
    (let-alist (unrepl--connection)
      (delete-process .process)
      (setq unrepl-connections (-remove (lambda (conn)
                                          (eq conn (unrepl--connection)))
                                        unrepl-connections))
      (if (eq unrepl-buffer-type 'repl)
          (with-current-buffer .output-buffer
            (setq unrepl-buffer-type nil)
            (kill-buffer .output-buffer))
        (with-current-buffer .repl-buffer
          (setq unrepl-buffer-type nil)
          (kill-buffer .repl-buffer))))))

(add-hook 'kill-buffer-hook 'unrepl--kill-buffer-hook-handler)

(provide 'unrepl)
;;; unrepl.el ends here
