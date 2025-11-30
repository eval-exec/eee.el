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
     ("ghostty" . "--title=ee-ghostty --window-decoration=none")
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

;; (defun ee-start-external-terminal (name command callback)
;;   "Start a process running COMMAND in an external terminal.
;; The terminal emulator is specified in `ee-terminal-command'.
;; See `ee-start-terminal-function' for the usage.
;; "
;;   (let* ((options (ee-get-terminal-options))
;;           ;; TODO: sleep 1 is a workaround,
;;           ;; should caught the error message
;;           ;; and show it in Emacs's echo area
;;           ;; errno 130 is indicates that a command or process was terminated by the user
;;           (full-command (format "%s %s -e bash -c '%s || { [ $? -ne 130 ] && sleep 1; }'"
;;                           ee-terminal-command
;;                           options
;;                           command))
;;           (proc (progn
;;                   (ee-message "ee-executing:\n%s" full-command)
;;                   (start-process-shell-command name nil full-command) )))
;;     (set-process-sentinel
;;       proc
;;       (lambda (p _m)
;;         (unless (process-live-p p)
;;           (ee-message "executing callback...")
;;           (funcall callback p))))
;;     proc))

(defun ee-start-external-terminal (name command callback)
  "Start a process running COMMAND in an external terminal."
  (let* ((options (split-string (ee-get-terminal-options)))
         (bash-command (format "%s || { [ $? -ne 130 ] && sleep 1; }" command))
         (args (append options (list "-e" "bash" "-c" bash-command))))
    (ee-message "ee-executing: %s %s"
                ee-terminal-command
                (string-join args " "))
    (apply #'async-start-process
           name
           ee-terminal-command
           callback
           args)))

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
    (ee--normalize-path (or (when-let (project (project-current))
                              (project-root project))
                          (shell-command-to-string project-dir-command) ))))


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
      (message "ee-jump got destination %s" destination)
      (let ((parts (list (format "file:%s" file))))
        (when line
          (push (format "line:%s" line) parts))
        (when pdf-page-num
          (push (format "page:%s" pdf-page-num) parts))
        (when column
          (push (format "column:%s" column) parts))
        (message "ee-jump jumping to: %s" (string-join (nreverse parts) ", ")))
      (redisplay 1)


      (funcall-interactively #'find-file file)

      (redisplay 1)
      (when line
        (goto-line line)
        (when column
          (move-to-column column)
          (recenter)))
      (when pdf-page-num
        (pdf-view-goto-page pdf-page-num))
      (select-frame-set-input-focus (selected-frame))
      (ignore-errors
        (jit-lock-fontify-now (window-start) (window-end)))
      ))
  )

;; destination-file is a temporary file, it's content is the desitination we want to jump
(defun ee-jump-from (destination-file)
  (when (file-exists-p destination-file)
    (let ((destination (string-trim
                         (with-temp-buffer
                           (insert-file-contents destination-file)
                           (buffer-string)))))
      (unless (string-empty-p destination)
        (ee-jump destination)))))

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
    (discard-input)
    (ee-start-process-shell-command-in-terminal
      name
      full-command
      process-callback)))


;;;###autoload
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


(defun ee-region-text()
  (when (use-region-p)
    (prog1
      (buffer-substring-no-properties
        (region-beginning)
        (region-end))
      (deactivate-mark))))


;;;;;; define ee commands here: ee-rg, ee-line, ee-yazi, etc. ;;;;;;;;;;;

;; Define your commands using the macro
;;;###autoload
(ee-define "ee-line" default-directory (ee-script-path "eee-line.sh")
  (list buffer-file-name (ee-region-text)) ee-jump-from)

;;;###autoload
(ee-define "ee-find" (ee-get-project-dir-or-current-dir) (ee-script-path "eee-find.sh") (list (ee-region-text)) ee-jump-from)

;;;###autoload
(ee-define "ee-lazygit" default-directory (ee-script-path "eee-lazygit.sh") nil ignore)

;;;###autoload
(ee-define "ee-rg" (ee-get-project-dir-or-current-dir) (ee-script-path "eee-rg.sh") (list (ee-region-text)) ee-jump-from)

;;;###autoload
(ee-define "ee-rga" default-directory (ee-script-path "eee-rga.sh")
  (list
    (expand-file-name
      (or buffer-file-name default-directory))
    (ee-region-text)
    )
  ee-jump-from)

;;;###autoload
(ee-define "ee-lf" default-directory (ee-script-path "eee-lf.sh") nil ee-jump-from)

;;;###autoload
(ee-define "ee-lf-project" (ee-get-project-dir-or-current-dir) (ee-script-path "eee-lf.sh") nil ee-jump-from)

;; ";" is important commmand should be "htop;" since, ee-run will redirect stdout to ee-process-output-file
;; if command is "htop", ee-run will redirect htop's stdout to ee-process-output-file,
;; then you will see nothing in launched teriminal
;;;###autoload
(ee-define "ee-htop" default-directory "htop;" nil ignore)
(ee-define "ee-btop" default-directory "btop;" nil ignore)

;; Commands with optional arguments
;;;###autoload
(ee-define "ee-yazi" default-directory (ee-script-path "eee-yazi.sh") (list buffer-file-name) ee-jump-from)

;; Eval Exec enhanced bones;

;;;###autoload
(ee-define "ee-bones" (ee-get-project-dir-or-current-dir) (ee-script-path "eee-bones.sh") nil ee-jump-from)

;;;###autoload
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
;;;###autoload
(ee-define "ee-delta"
  (ee-get-project-dir-or-current-dir)
  "git -c core.diff=delta -c delta.pager=less -c delta.paging=always diff ; echo fjeio "
  nil
  ignore
  )

;;;###autoload
(ee-define "ee-spotify-player"
  default-directory
  "spotify_player;"
  nil
  ignore
  )

(defun ee-recentf-dump ()
  (interactive)
  (let ((result (string-join recentf-list "\n")))
    (f-write-text result 'utf-8 "/tmp/ee-recentf-list.txt")
    ))


;;;###autoload
(ee-define "ee-recentf"
  (progn
    (ee-recentf-dump)
    (ee-get-project-dir-or-current-dir)
    )
  (ee-script-path "eee-recentf.sh")
  nil
  ee-jump-from
  )

;;;###autoload
(ee-define "ee-project-switch" default-directory
  "fd --color=always --type dir --exact-depth 3 \"\"  ~/Projects | fzf --ansi --exact --style full --cycle --layout reverse --preview \"eza --tree -L 3 --color always --icons always {}\" "
  nil ee-jump-from)

(defvar ee-keymap (make-sparse-keymap)
  "Keymap for ee-* commands.")
(define-key ee-keymap (kbd "f") 'ee-find)
(define-key ee-keymap (kbd "g") 'ee-lazygit)
(define-key ee-keymap (kbd "y") 'ee-yazi)
(define-key ee-keymap (kbd "Y") 'ee-yazi-project)
(define-key ee-keymap (kbd "r") 'ee-rg)
(define-key ee-keymap (kbd "l") 'ee-line)


(provide 'eee)
