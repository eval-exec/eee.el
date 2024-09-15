;;; eee-vterm.el --- VTerm support for EEE           -*- lexical-binding: t; -*-

;;; Commentary:

;; To use VTerm with eee, eval the following code:

;;    (setq ee-start-terminal-function #'ee-vterm-start-terminal)

;;; Code:

(require 'vterm)

(define-derived-mode ee-vterm-mode vterm-mode "VTerm[EEE]")

;;;###autoload
(defun ee-vterm-start-terminal (name command callback)
  (let* ((dir default-directory)
	 (vterm-shell (format "sh -c %s" (shell-quote-argument command)))
	 (vterm-buffer-name (concat "*" name "*"))
	 (vterm-kill-buffer-on-exit nil))
    (with-current-buffer (get-buffer-create vterm-buffer-name)
      (setq default-directory dir)
      (pop-to-buffer (current-buffer)
		     '((display-buffer-pop-up-frame)))
      (ee-vterm-mode)
      (set-process-sentinel
       (get-buffer-process (current-buffer))
       (let ((window (selected-window)))
	 (lambda (p _m)
	   (unless (process-live-p p)
	     (when (window-live-p window)
	       (quit-restore-window window 'kill))
	     (funcall callback p))))))))

(provide 'eee-vterm)
;;; eee-vterm.el ends here
