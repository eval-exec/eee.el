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

;; enable ee-function's debug message?
(defcustom ee-debug-message nil
  "if t, then eee.el will print debug message to message buffer"
  :type 'boolean
  :group 'eee)

(defun ee-message (format-string &rest args)
  "Call `message` with FORMAT-STRING and ARGS if `ee-debug-message` is non-nil."
  (when ee-debug-message
    (apply 'message format-string args)))


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
	     (full-command (format "%s %s -e bash -c '%s'"
			                   ee-terminal-command
			                   options
							   command))
	     (proc (progn
				 (ee-message "ee-executing:\n%s" full-command)
				 (start-process-shell-command name nil full-command) )))
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


(defun ee--normalize-path (path)
  (string-trim-right path (rx (or "\n" "\\" "/"))))

(defun ee-get-project-dir-or-current-dir()
  (let ((project-dir-command
		 (format  "git rev-parse --show-toplevel 2> /dev/null || echo -n %s"
                  default-directory)))
    (ee--normalize-path (shell-command-to-string project-dir-command))))


(defun ee-integer-p(str)
  (when str
	(string-match-p "^[-+]?[0-9]+$" str)))

(defun ee-jump (destination)
  "Jump to DESTINATION, which can be a file path with optional line and column numbers.
DESTINATION can be:
- /path/to/file
- /path/to/file:12
- /path/to/file:12:7
- /path/to/some-pdffile.pdf:Page 123: bala bala bala
"
  (interactive "sEnter destination: ")
  (let* ((components (split-string destination ":"))
         (file (car components))
         (line (if (and (length> components 1)
						(ee-integer-p (nth 1 components)))
				   (string-to-number (nth 1 components)) nil))
		 ;; pdf-page is the page number in pdf file, like: "Page 123", parse page number to pdf-page
		 (pdf-page-num 
		  (when (and
				 (length> components 1)
				 (string-match "Page \\([0-9]+\\)" (nth 1 components)))
			(string-to-number (match-string 1 (nth 1 components)))))

         (column (if (and (length> components 2) (ee-integer-p (nth 2 components)))
					 (string-to-number (nth 2 components)) nil)))
	(when (and (not (string-empty-p file)) (file-exists-p file))
	  (message "ee-jump get %s; going jump to: file:%s, [line:%s/page:%s], column:%s" destination file line pdf-page-num column)
      (find-file file)
      (when line
		(goto-line line)
		(when column
          (move-to-column column)
		  (recenter)))
	  (when pdf-page-num
		(pdf-view-goto-page pdf-page-num)))))

;; destination-file is a temporary file, it's content is the desitination we want to jump 
(defun ee-jump-from(destination-file)
  (let* ((destination (shell-command-to-string
					   (format "cat %s" destination-file)))
		 (destination (string-trim destination)))
	(unless (string-empty-p destination)
      (ee-jump destination))))

(defun ee-join-args(args)
  (string-join
   (mapcar (lambda(str)
			 (if str (shell-quote-argument str) ""))
		   args) " "))

(defun ee-run(name working-directory command &optional args callback)
  ;; name is the process name, it's needed by `start-process-shell-command's first argument
  ;; working-directory: after launch teriminal, it will cd to working-directory, then execute commands
  ;; command: the command to execute, should be a string, like: "yazi", "rg", "git", etc.
  ;; args: optional, should be list of string,  the arguments for the command. like: '("arg1" "arg2" "arg3")'
  ;; callback, optional, the callback function to call after the command is executed, it will accept ee-process-output-file as argument
  (let* ((working-directory
		  (shell-quote-argument
		   (expand-file-name  working-directory)))
		 ;; pre-define ee-* commands' output file, callback function will read content from the file
		 (ee-process-stdout-file (format "/tmp/ee-stdout-%s.tmp" name))
		 ;; construct command and args to execute in terminal:
		 (command-and-args
		  (format "%s %s" command
				  (ee-join-args args)))
		 ;; example:
		 ;; cd [working-directory] && [command-and-args] > [/tmp/ee-output-ee-rg.tmp]
		 (full-command (format "cd %s && %s > %s"
							   working-directory
							   command-and-args
							   ee-process-stdout-file))
		 (process-callback (if callback
							   (lambda(process)
								 (funcall callback ee-process-stdout-file))
							 #'ignore)))
	(ee-message "%s: %s" name full-command)
	(ee-start-process-shell-command-in-terminal
	 name
	 full-command
	 process-callback)))

(defmacro ee-define (name working-directory script-path &optional args callback)
  "Define a command with NAME to run a script with EE-RUN.
WORKING-DIRECTORY determines the directory to run the script from.
SCRIPT-PATH is the full path to the script to run.
ARGS are optional arguments for the script.
CALLBACK is an optional callback to be called after the script runs."
  `(defun ,(intern name) (&optional _arg)
     (interactive "P")
     (ee-run ,name
             ,working-directory
             ,script-path
             ,args
             ',callback)))


;;;;;; define ee commands here: ee-rg, ee-line, ee-yazi, etc. ;;;;;;;;;;;

;; Define your commands using the macro
(ee-define "ee-line" default-directory (ee-script-path "eee-line.sh")
           (list buffer-file-name) ee-jump-from)

(ee-define "ee-find" (ee-get-project-dir-or-current-dir) (ee-script-path "eee-find.sh") nil ee-jump-from)

(ee-define "ee-lazygit" default-directory (ee-script-path "eee-lazygit.sh") nil ignore)

(ee-define "ee-rg" default-directory (ee-script-path "eee-rg.sh") nil ee-jump-from)

(ee-define "ee-rga" default-directory (ee-script-path "eee-rga.sh")
		   (list
			(expand-file-name
			 (or buffer-file-name default-directory)))
		   ee-jump-from)

(ee-define "ee-lf" default-directory (ee-script-path "eee-lf.sh") nil ee-jump-from)

(ee-define "ee-lf-project" (ee-get-project-dir-or-current-dir) (ee-script-path "eee-lf.sh") nil ee-jump-from)

;; ";" is important commmand should be "htop;" since, ee-run will redirect stdout to ee-process-output-file
;; if command is "htop", ee-run will redirect htop's stdout to ee-process-output-file,
;; then you will see nothing in launched teriminal
(ee-define "ee-htop" default-directory "htop;" nil ignore)
(ee-define "ee-btop" default-directory "btop;" nil ignore)

;; Commands with optional arguments
(ee-define "ee-yazi" default-directory (ee-script-path "eee-yazi.sh") (list buffer-file-name) ee-jump-from)

(ee-define "ee-yazi-project"
           (ee-get-project-dir-or-current-dir)
           (ee-script-path "eee-yazi.sh")
		   (if
			   (and
				(equal
				 (ee--normalize-path default-directory)
				 (ee-get-project-dir-or-current-dir))
				buffer-file-name)
			   (list buffer-file-name)
			 (list (ee-get-project-dir-or-current-dir)))
		   ee-jump-from)

;; use delta to show git diff
(ee-define "ee-delta"
		   (ee-get-project-dir-or-current-dir)
		   "git -c core.diff=delta -c delta.pager=less -c delta.paging=always diff ; echo fjeio "
		   nil
		   ignore
		   )

(provide 'eee)
