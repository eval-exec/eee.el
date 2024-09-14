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


;; specify which terminal for ee-* commands to use. alacritty, or wezterm?
(defcustom ee-terminal-command "wezterm"
  "The terminal command to use for ee-* commands."
  :type 'string
  :group 'eee)

(defcustom ee-terminal-options
  '(("wezterm" .
	 "--config enable_wayland=false --config enable_tab_bar=false --config initial_cols=180 --config initial_rows=50 --config window_decorations=\\\"NONE\\\"")
	("alacritty" .
	 "--option=window.decorations=\\\"None\\\" --option=window.dimensions.columns=180 --option=window.dimensions.lines=50"))
  "The terminal command options to use for ee-* commands."
  :type 'alist
  :group 'eee)


(defun ee-get-teriminal-options()
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

(defun ee-find-file(target-file)
  (message "ee-find-file: %s" target-file)
  (when (not (string-empty-p target-file))
	(find-file (string-trim target-file))))


(defun ee-get-project-dir-or-current-dir()
  (let* ((project-dir-command
		  (format  "git rev-parse --show-toplevel 2> /dev/null || echo -n %s" default-directory))
		 (cwd (string-trim-right (shell-command-to-string project-dir-command) "\n")))
	cwd))


(defun ee-find--sentinel(process event)
  (cond
   ((string= event "finished\n")
	(let* ((target-file (shell-command-to-string "cat /tmp/ee-find.tmp"))
		   (target-file (string-trim target-file)))
	  (when (not (string-empty-p target-file))
		(message "ee-find opening: %s" target-file)
		(ee-find-file target-file)
		)
	  ))
   (t
	(message "ee-find: Event is not finished: %s" event)))
  )


;; Eval Exec find file in project or current dir
(defun ee-find()
  (interactive)
  (let* ((options (ee-get-teriminal-options))
		 (working-directory (ee-get-project-dir-or-current-dir))
		 (command
		  (format "%s > /tmp/ee-find.tmp"
				  (ee-script-path "eee-find.sh")))
		 (full-command (format "%s %s -e bash -c 'cd %s && %s'"
							   ee-terminal-command
							   options
							   working-directory
							   command))
		 (ee-find-process
		  (start-process-shell-command "ee-find" nil full-command)))
	;; (message "ee-find: %s" full-command)
	(set-process-sentinel ee-find-process #'ee-find--sentinel)))



(defun ee-yazi--sentinel (process event)
  (cond
   ((string= event "finished\n")
	(let* ((target-file (shell-command-to-string "cat /tmp/ee-yazi.tmp"))
		   (target-file (string-trim target-file)))
	  (when (not (string-empty-p target-file))
		(message "ee-yazi opening: %s" target-file)
		(ee-find-file target-file))))
   (t
	(message "Event is not finished: %s " event))))

;; Eval Exec 鸭子 yazi https://github.com/sxyazi/yazi in current dir
(defun ee-yazi-in(dir)
  (let* ((command (ee-script-path "eee-yazi.sh"))
		 (options (ee-get-options))
		 (full-command
		  (format "%s %s -e bash -c 'cd %s && %s'"
				  ee-terminal-command
				  options
				  dir
				  command))
		 (ee-yazi-process (start-process-shell-command "ee-yazi" nil full-command))
		 )
	;; (message "ee-yazi executing: %s" full-command)
	(set-process-sentinel ee-yazi-process #'ee-yazi--sentinel)
	))


;; Eval Exec 鸭子 yazi in current dir
(defun ee-yazi()
  (interactive)
  (ee-yazi-in default-directory))

;; Eval Exec 鸭子 yazi in current project dir
(defun ee-yazi-project()
  (interactive)
  (ee-yazi-in (ee-get-project-dir-or-current-dir)))


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

(defun ee-rg-sentinel(process event)
  (message "ee-rg-sentinel: %s" event)
  (cond
   ((string= event "finished\n")
	(let* ((ee-rg-result (shell-command-to-string "cat /tmp/ee-rg.tmp")))
	  (message "ee-rg get: %s" ee-rg-result)
	  (ee-find-file-at-line-and-column ee-rg-result)))
   (t
	(message "ee-rg: Event is not finished: %s" event)))
  )

(defun ee-rg()
  (interactive)
  (let* ((options (ee-get-teriminal-options))
		 (working-directory (ee-get-project-dir-or-current-dir))
		 (command (format "%s > /tmp/ee-rg.tmp"
						  (ee-script-path "eee-rg.sh")))
		 (full-command (format
						"%s %s -e bash -c 'cd %s && %s'"
						ee-terminal-command
						options
						working-directory
						command))
		 (ee-rg-process (start-process-shell-command "ee-rg" nil full-command))
		 )
	(set-process-sentinel ee-rg-process #'ee-rg-sentinel)))


;;; Eval Exec execute lazygit
(defun ee-lazygit()
  (interactive)
  (let* ((command  (ee-script-path "eee-lazygit.sh"))
		 (options (ee-yazi-wezterm-options))
		 (full-command
		  (format "wezterm %s -e bash -c %s"
				  options
				  command)))
	;; (message "ee-lazygit: %s" full-command)
	(start-process-shell-command "ee-lazygit" nil full-command)))

(defun ee-line-sentinel(process event)
  (message "ee-line-sentinel: %s" event)
  (cond
   ((string= event "finished\n")
	(let* ((target-file (shell-command-to-string "cat /tmp/ee-line.tmp"))
		   (target-file (string-trim target-file))
		   )
	  (when (not (string-empty-p target-file))
		(message "ee-line opening: %s" target-file)
		(ee-find-file-at-line-and-column target-file))))
   (t
	(message "ee-line: Event is not finished: %s" event)))

  )

;;; Eval Exec search line
(defun ee-line()
  (interactive)
  (let* ((command
		  (ee-script-path "eee-line.sh"))
		 (options (ee-yazi-wezterm-options))
		 (full-command
		  (format "wezterm %s -e bash -c %s"
				  options
				  (shell-quote-argument
				   (format "%s %s"
						   command
						   (buffer-file-name)))))
		 (ee-line-process (start-process-shell-command "ee-line" nil full-command)))

	(set-process-sentinel ee-line-process #'ee-line-sentinel)))

(provide 'eee)
