;;; eee-eat.el --- Eat support for eee.el -*- lexical-binding: t -*-

;;; Commentary:

;; To use Eat with EEE, eval the following code:

;;   (setq ee-start-terminal-function #'ee-eat-start-terminal)

;;; Code:

(require 'eat)

;; Define a derived mode of eat-mode to make it easy
;; to customize `display-buffer-alist'.
(define-derived-mode ee-eat-mode eat-mode "Eat[EEE]" )

(defun ee-eat--setup-buffer ()
  (let ((inhibit-read-only t))
    (erase-buffer))
  (unless (eq major-mode #'ee-eat-mode)
    (ee-eat-mode))
  (pop-to-buffer (current-buffer)
		 '((display-buffer-pop-up-frame)))
  ;; Enable `eat-char-mode' so that all keyboard events are sent to Eat.
  ;; `eat-char-mode' can only be enabled after the process starts.
  (add-hook 'eat-exec-hook (lambda (_) (eat-char-mode)) nil t)

  ;; Delete the popped-up frame after the process exits.  Use a higher
  ;; priority to make sure this function executes before the callback function
  (add-hook 'eat-exit-hook (lambda (p)
			     (when (zerop (process-exit-status p))
			       (quit-restore-window (selected-window) 'kill)))
	    -90 t))

;;;###autoload
(defun ee-eat-start-terminal (name command callback)
  "Run COMMAND in an Eat buffer and display that buffer in a new frame.

The buffer name is made by surrounding NAME with `*'s.

The buffer is killed if the process exits normally.
CALLBACK is called after the process exits.

See `ee-start-terminal-function' and
`ee-start-process-shell-command-in-terminal'."
  (let ((dir default-directory))
    (with-current-buffer (get-buffer-create (concat "*" name "*"))
      (setq default-directory dir)
      (ee-eat--setup-buffer)
      (add-hook 'eat-exit-hook callback 50 t)
      (eat-exec (current-buffer) name "bash" nil (list "-c" command)))))

(provide 'eee-eat)
;;; eee-eat.el ends here
