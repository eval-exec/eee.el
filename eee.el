;;; package --- Summary -*- lexical-binding: t -*-
;; eee.el is the meaning of Eval! Exec! Enhance!
;;
;; Author Github: https://github.com/eval-exec
;; Author Blog:   https://evex.one
;;
;; Eval! Exec! Enhance!
;; Eval! Exec! Excited!
;; Eval! Exec! Enjoy!
;; Eval! Exec! Emacs!
;; Unite! all Emacs users worldwide!
;; Make Emacs Great Again!

;; specify which terminal for ee-* commands to use. alacritty, wezterm, kitty, or konsole?
(defcustom ee-terminal-command "wezterm"
  "The terminal command to use for ee-* commands."
  :type 'string
  :group 'eee)

(defcustom ee-terminal-options
  '(("wezterm" .
	 "--config enable_wayland=false --config enable_tab_bar=false --config initial_cols=180 --config initial_rows=50 --config window_decorations=\\\"NONE\\\"")
	("alacritty" .
	 "--option=window.decorations=\\\"None\\\" --option=window.dimensions.columns=180 --option=window.dimensions.lines=50")
	("kitty" . "--title ee-kitty")
	("konsole" . "--hide-menubar")
	)
  "The terminal command options to use for ee-* commands."
  :type 'alist
  :group 'eee)


(defun ee-get-terminal-options()
  (alist-get
   ee-terminal-command
   ee-terminal-options "" nil 'equal))

(defvar eee--load-file-path nil)
(setq eee--load-file-path (or load-file-name buffer-file-name))

(defun ee-script-path(script-name)
  ;; script-name is in same dir with current eee.el script
  (expand-file-name
   script-name
   (file-name-directory (expand-file-name eee--load-file-path))))

(defun ee-start-external-terminal (name command callback)
  "Start a process running COMMAND in an external terminal.
The terminal emulator is specified in `ee-terminal-command'.
See `ee-start-terminal-function' for the usage.
"
  (let* ((options (ee-get-terminal-options))
	       (full-command (format "%s %s -e bash -c %s"
			                         ee-terminal-command
			                         options
			                         (shell-quote-argument command)))
	       (proc (start-process-shell-command name nil full-command)))
    (set-process-sentinel
     proc
     (lambda (p _m)
       (unless (process-live-p p)
	       (funcall callback p))))
    proc))

(defcustom ee-start-terminal-function #'ee-start-external-terminal
  "Function used to start the terminal.
See `ee-start-external-terminal' for function signature."
  :type '(choice (const :tag "External terminal" ee-start-external-terminal)
		             (const :tag "Eat" ee-eat-start-terminal)
		             (function "Custom function")))

(defun ee-start-process-shell-command-in-terminal (name command callback)
  "Run COMMAND in a terminal.

This function uses `ee-start-terminal-function' to start the terminal.

CALLBACK is called with the process as the single argument when the process
exits.

NAME is passed to `ee-start-terminal-function'."
  (funcall ee-start-terminal-function name command callback))

(defvar ee-find-file--actions
  '(("find-file" . find-file)
    ("browse-url" . browse-url)))

(defun ee-find-file (target-file)
  (message "ee-find-file: %s" target-file)
  (let* ((default-directory (ee-get-project-dir-or-current-dir)))
    (when (not (string-empty-p target-file))
      (if (not current-prefix-arg)
          (find-file (string-trim target-file))
        (let ((action-fn
               (alist-get (completing-read "Action:" ee-find-file--actions)
                          ee-find-file--actions nil nil 'equal)))
          (funcall action-fn target-file))))))

(defun ee--normalize-path (path)
  (string-trim-right path (rx (or "\n" "\\" "/"))))

(defun ee-get-project-dir-or-current-dir()
  (let ((project-dir-command
		     (format  "git rev-parse --show-toplevel 2> /dev/null || echo -n %s"
                  default-directory)))
    (ee--normalize-path (shell-command-to-string project-dir-command))))

(defun ee-find--callback (_process)
  (let* ((target-file (shell-command-to-string "cat /tmp/ee-find.tmp"))
	       (target-file (string-trim target-file)))
    (when (not (string-empty-p target-file))
      (message "ee-find opening: %s" target-file)
      (ee-find-file target-file))))


;; Eval Exec find file in project or current dir
(defun ee-find (&optional _arg)
  (interactive "P")
  (let* ((working-directory (ee-get-project-dir-or-current-dir))
	       (command
	        (format "cd %s && %s > /tmp/ee-find.tmp"
		              working-directory
		              (ee-script-path "eee-find.sh"))))
    (ee-start-process-shell-command-in-terminal "ee-find" command #'ee-find--callback)))


(defun ee-lf--callback (_process)
  (let* ((target-file (shell-command-to-string "cat /tmp/ee-lf.tmp"))
         (target-file (string-split target-file "\n" t)))
    (dolist (file target-file)
      (when (not (string-empty-p file))
        (message "ee-lf-opening: %s" file)
        (ee-find-file file)))))

(defun ee-lf-in (dir)
  (let* ((command (ee-script-path "eee-lf.sh"))
         (full-command (format "cd %s && %s" dir command)))
    (ee-start-process-shell-command-in-terminal
     "ee-lf" full-command #'ee-lf--callback)))

(defun ee-lf (&optional _arg)
  (interactive "P")
  (ee-lf-in default-directory))

(defun ee-lf-project (&optinoal _arg)
  (interactive "P")
  (ee-lf-in (ee-get-project-dir-or-current-dir)))

(defun ee-yazi--callback (_process)
  (let* ((target-file (shell-command-to-string "cat /tmp/ee-yazi.tmp"))
	       (target-file (string-trim target-file)))
    (when (not (string-empty-p target-file))
      (message "ee-yazi opening: %s" target-file)
      (ee-find-file target-file))))

;; Eval Exec 鸭子 yazi https://github.com/sxyazi/yazi in current dir
(defun ee-yazi-open (path)
  (let* ((yazi (ee-script-path "eee-yazi.sh"))
	       (full-command (concat yazi " " path)))
    (ee-start-process-shell-command-in-terminal
     "ee-yazi" full-command #'ee-yazi--callback)))


;; Eval Exec 鸭子 yazi in current dir
(defun ee-yazi (&optional _arg)
  (interactive "P")
  (if buffer-file-name
      (ee-yazi-open buffer-file-name)
    (ee-yazi-open default-directory)))

;; Eval Exec 鸭子 yazi in current project dir
(defun ee-yazi-project (&optional _arg)
  (interactive "P")
  (let ((project-directory (ee-get-project-dir-or-current-dir)))
    (if (and (equal (ee--normalize-path default-directory) project-directory)
             buffer-file-name)
        (ee-yazi-open buffer-file-name)
      (ee-yazi-open project-directory))))


(defun ee-find-file-at-line-and-column (string)
  (when (string-match "\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):" string)
	(let ((file (match-string 1 string))
		  (line (string-to-number (match-string 2 string)))
		  (column (string-to-number (match-string 3 string))))
	  (message "ee-find file: %s line: %s column: %s" file line column)
	  (ee-find-file file)
	  (goto-line line)
	  (move-to-column (1- column))
	  (recenter))))

(defun ee-rg--callback (process)
  (let* ((ee-rg-result (shell-command-to-string "cat /tmp/ee-rg.tmp")))
    (message "ee-rg get: %s" ee-rg-result)
    (ee-find-file-at-line-and-column ee-rg-result)))

(defun ee-rg ()
  (interactive)
  (let* ((working-directory (ee-get-project-dir-or-current-dir))
	       (command (format "cd %s && %s > /tmp/ee-rg.tmp"
			                    working-directory
			                    (ee-script-path "eee-rg.sh"))))
    (ee-start-process-shell-command-in-terminal
     "ee-rg" command #'ee-rg--callback)))


;;; Eval Exec execute lazygit
(defun ee-lazygit()
  (interactive)
  (let* ((command  (ee-script-path "eee-lazygit.sh")))
    (ee-start-process-shell-command-in-terminal "ee-lazygit" command #'ignore)))

(defun ee-line--callback(process)
  (let* ((target-file (shell-command-to-string "cat /tmp/ee-line.tmp"))
	 (target-file (string-trim target-file)))
    (when (not (string-empty-p target-file))
      (message "ee-line opening: %s" target-file)
      (ee-find-file-at-line-and-column target-file))))

;;; Eval Exec search line
(defun ee-line ()
  (interactive)
  (let* ((command
	        (format "%s %s" (ee-script-path "eee-line.sh")
		              (buffer-file-name))))
    (ee-start-process-shell-command-in-terminal "ee-line" command #'ee-line--callback)))

(provide 'eee)
