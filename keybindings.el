(use-package emacs
  :bind (("M-#" . define-word)
	 ("C-M-3" . define-word-at-point)
	 ("C-c f n" . insert-file-name)
	 ("C-c s i r" . string-insert-rectangle)
	 ("C-c s r" . insert-scripture-ref)
	 ("C-x O" . previous-multiframe-window)
	 ("C-x {" . sticky-shrink-window-horizontally)
	 ("C-x }" . sticky-enlarge-window-horizontally)
	 ("C-c d" . insert-date)
	 ("s-<return>" . toggle-frame-fullscreen)
	 ("s-u" . toggle-frame-transparency)
	 ("s-}" . tab-next)
	 ("s-{" . tab-previous)
	 ("s-t" . tab-new)))

(defvar transparency--toggle-var t)
(defun toggle-frame-transparency ()
  "Toggle the transparency of all frames."
  (interactive)
  (if transparency--toggle-var
      (set-frame-parameter (selected-frame) 'alpha '(83 83))
    (set-frame-parameter (selected-frame) 'alpha '(100 100)))
  (setq transparency--toggle-var (not transparency--toggle-var)))

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

(defun insert-file-name ()
  (interactive)
  (insert (file-name-base buffer-file-truename)))

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
