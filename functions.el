;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Emacs Functions					 ;;
;; 								 ;;
;; Author: Ashton Wiersdorf					 ;;
;; 								 ;;
;; What follows is a list of my personal functions for Emacs. No ;;
;; warranty. Good luck decoding the regexes.			 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun latexify-quotes ()
  "Replace the quotation marks with latex-compatable marks so you get the nice curly marks. Operates on the entire buffer."
  (interactive)
  (save-excursion
    (progn
      (goto-char (point-min))
      (replace-regexp "\"\\(\\w\\)" "``\\1")
      (goto-char (point-min))
      (replace-regexp "\\([[:alnum:][:punct:]]\\)\"" "\\1''"))))

(defvar transparency--toggle-var t)
(defun toggle-frame-transparency ()
  "Toggle the transparency of all frames."
  (interactive)
  (if transparency--toggle-var
      (set-frame-parameter (selected-frame) 'alpha '(83 83))
    (set-frame-parameter (selected-frame) 'alpha '(100 100)))
  (setq transparency--toggle-var (not transparency--toggle-var)))

(defun define-keys-globally (func &rest binds)
  "Define one or several keys to a particular function in the global keymap.

Example:

(define-keys-globally 'magit-status \"C-x g\" \"s-g\")"
  (mapc (lambda (bind) (define-key global-map (kbd bind) func)) binds))

;; Pandoc conversion functions (for Sarah)
;; Requires f.el
(defcustom pandoc-converter-args "--filter pandoc-citeproc --pdf-engine=xelatex" "Additional arguments to pass to pandoc when running `convert-with-pandoc'")

(defun convert-with-pandoc ()
  "Convert a file between formats with Pandoc.

This will place the outputted function in the same directory as
the source folder.

Opens the file in a new window if the output format is a plain-text format."
  (interactive)

  (require 'f)
  (let* ((in-file (read-file-name "File to convert: " nil nil t))
	 (out-format (completing-read "Output format: " '("md" "docx" "html" "org" "txt" "pdf")))
	 (out-file (f-swap-ext in-file out-format)))
    (cd (f-dirname in-file))
    (shell-command (concat "pandoc " pandoc-converter-args " \"" (f-filename in-file) "\" -o \"" (f-filename out-file) "\""))
    (if (member out-format '("md" "txt" "html" "org"))
	(find-file (f-filename out-file)))))

;; Consider creating a variable that remembers the last directory given
;; to the rg-subdir function. That way, on successive calls the user
;; could keep using the subdirectory.
;;
;; Alternativley, find a way to add a counsel command to limit searches
;; to a particular subdirectory.

(defvar counsel-projectile-rg-last-directory nil "Last directory searched with `counsel-projectile-rg-subdir'.")

(defun counsel-projectile-rg-subdir (prefix)
  "Search the current project with rg.

If called with a prefix argument, prompt the user for a
subdirectory of the project to search.

If called with two prefix arguments (`C-u C-u`) prompt user for
the subdirectory AND extra arguments to pass to rg."
  (interactive "P")
  (if (and (eq projectile-require-project-root 'prompt)
           (not (projectile-project-p)))
      (counsel-projectile-rg-action-switch-project)
    (let* ((ivy--actions-list (copy-sequence ivy--actions-list))
           (dir-to-search (if prefix
                              (read-file-name "Subdirectory to search: "
                                              (or counsel-projectile-rg-last-directory (projectile-project-root))
                                              nil
                                              t "" #'directory-name-p)
                            (or counsel-projectile-rg-last-directory
                                (projectile-project-root))))
           (extra-args (if (equal prefix '(16)) (read-string "Extra arguments for rg: ")))
           (ignored
            (mapconcat (lambda (i)
                         (concat "--glob !" (shell-quote-argument i)))
                       (append
                        (projectile--globally-ignored-file-suffixes-glob)
                        (projectile-ignored-files-rel)
                        (projectile-ignored-directories-rel))
                       " "))
           (counsel-rg-base-command
            (let ((counsel-ag-command counsel-rg-base-command))
              (counsel--format-ag-command ignored "%s"))))

      (if prefix
          (setq counsel-projectile-rg-last-directory dir-to-search))

      (ivy-add-actions
       'counsel-rg
       counsel-projectile-rg-extra-actions)
      (counsel-rg (eval counsel-projectile-rg-initial-input)
                  dir-to-search
                  extra-args
                  (projectile-prepend-project-name
                   (concat (car (if (listp counsel-rg-base-command)
                                    counsel-rg-base-command
                                  (split-string counsel-rg-base-command)))
                           ": "))))))

(defun install-my-packages ()
  (interactive)
  (dolist (pkg package-selected-packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(defun query-kill-string (arg)
  "Prompt for input in the minibuffer. Typed value is pushed onto
  the kill ring."
  (interactive "P")
  (let* ((prompt (if arg (read-from-minibuffer "PROMPT TO USE: ") "Input: "))
         (input (read-from-minibuffer prompt)))
    (kill-new input)))

(defun deft-this-directory (prefix)
  "Open deft on the current directory. (As returned by `default-directory`.)"
  (interactive "P")
  ;; Note: this takes advantage that Emacs Lisp is dynamically scoped.
  ;; Stupid, yes, but it's a feature we can use here.
  (let* ((deft-recursive (equal prefix '(4)))
         (deft-directory default-directory))
    (deft)))

(defun wrap-sexp ()
  "Wrap the s-expression at point in parens. Effectively the same
as running the sequence `C-M-<SPC> (`"
  (interactive)
  (save-excursion
    (insert-char ?\()
    (forward-sexp)
    (insert-char ?\))))

(defun insert-scripture-ref ()
  (interactive)
  ;; Pure-emacs solution: Look up "completing-read" and build a hash
  ;; table of all scriptures. Here's a link to the docs:
  ;; [[help:completing-read][completing-read]]
  (let ((ref (completing-scripture-read)))
    (insert (format "[[scrip:%s][%s]]" ref ref))))

(defun notes-this-day ()
  "Display files of the form '*-mm-dd*' in the current directory,
where 'mm-dd' are the current month and day."
  (interactive)
  (let* ((month-day (format-time-string "%m-%d"))
         (this-day-matching (concat "[[:digit:]]+-" month-day))
         (note-files-this-day (directory-files-recursively "." this-day-matching)))

    ;; make a buffer and fill it with the contents
    (let ((buff (generate-new-buffer "*Notes on this day*")))
      (set-buffer buff)                   ; Make this buffer current
      (org-mode)
      (insert "* Notes on this day *\n")
      (mapc (lambda (notes-file)
              (progn
                (insert "\n------------------------------------------------------------\n")
                (insert (concat "[[file:" notes-file "][" notes-file "]]"))          ; File name, as a hyperlink
                (insert "\n")
                (insert-file-contents notes-file)
                (end-of-buffer)))
            note-files-this-day)
      (read-only-mode)
      (display-buffer-pop-up-window buff nil))))

(defun toggle-auto-writer-mode ()
  (interactive)
  (setq auto-writer-mode (not auto-writer-mode))
  (display-message-or-buffer (format "Auto writer mode set to %s" auto-writer-mode)))

(defun writer-mode ()
  (interactive)
  (progn
    (darkroom-tentative-mode)
    (centered-cursor-mode)
    ;; (imenu-list-noselect)
    ;; (olivetti-mode)
    ))

(defun unfill-region (beg end)
  "Remove hard linebreaks from a region of text. Preserves two newlines in a row."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\n *\\([^\n]\\)" end t)
      (replace-match " \\1" nil nil))
    (goto-char beg)
    (while (re-search-forward "\n " end t)
      (replace-match "\n\n"))))

;;; Insert Date
(global-set-key (kbd "C-c d") 'insert-date)
(defun format-date (prefix)
    "Format the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (let ((format (cond
                   ((not prefix) "%F")
                   ((equal prefix '(4)) "%F %T")
                   ((equal prefix '(16)) "%A, %e %B %Y")))
          (system-time-locale))
          ;; (system-time-locale "de_DE"))
      (format-time-string format)))

(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (insert (format-date prefix)))

;;; call perltidy on region
(defun perltidy (beg end)
  "Call perltidy on a region"
  (interactive "r")
  (shell-command-on-region beg end "perltidy" nil t))

;;; copy region to system clipboard
(global-set-key (kbd "C-c c") 'system-copy)
(setq system-clipboard-program "pbcopy")
(defun system-copy (beg end)
  "Copy region to system clipboard. Command to which output will
be piped defined by `system-clipboard-program`"
  (interactive "r")
  (shell-command-on-region beg end system-clipboard-program nil nil))

;;; encrypt region
(defun encrypt-region (beg end)
  "Encrypt a region of a buffer (you will be prompted for a passphrase)"
  (interactive "r")
  (let (passphrase command)
    (setq passphrase (read-passwd "Passphrase: "))
    (setq command (concat "openssl enc -bf -a -e -pass pass:'" passphrase "'"))
    (shell-command-on-region beg end command (current-buffer) t)))

;;; decrypt region
(defun decrypt-region (beg end)
  "Decrypt a region of a buffer (you will be prompted for a passphrase)"
  (interactive "r")
  (let (passphrase command)
    (setq passphrase (read-passwd "Passphrase: "))
    (setq command (concat "openssl enc -bf -a -d -pass pass:'" passphrase "'"))
    (shell-command-on-region beg end command (current-buffer) t)))
;;(define-key global-map (kbd "C-c C-e") 'encrypt-region)
;;(define-key global-map (kbd "C-c C-r") 'decrypt-region)

(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setq make-backup-file-name-function 'my-backup-file-name)

(defun sticky-enlarge-window-horizontally (prefix)
  (interactive "P")
  (enlarge-window-horizontally (if prefix (car prefix) 1))
  (unless (current-message)
    (message "(Now hold `{' or `}' to adjust window size)"))
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "{") 'shrink-window-horizontally)
    (define-key map (kbd "}") 'enlarge-window-horizontally)
    (set-transient-map map t)))

(defun sticky-shrink-window-horizontally (prefix)
  (interactive "P")
  (shrink-window-horizontally (if prefix (car prefix) 1))
  (unless (current-message)
    (message "(Now hold `{' or `}' to adjust window size)"))
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "{") 'shrink-window-horizontally)
    (define-key map (kbd "}") 'enlarge-window-horizontally)
    (set-transient-map map t)))

(defun insert-file-name ()
  (interactive)
  (insert (file-name-base buffer-file-truename)))

(defun open-current-file-macdown ()
  (interactive)
  (shell-command (concat "macdown \"" buffer-file-name "\"")))

(defun my-racket-doc (prefix)
  (interactive "P")
  (cond ((not prefix) (racket-xp-describe))
        ((equal prefix '(4)) (racket-doc))))
