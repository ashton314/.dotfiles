(use-package emacs
  :bind (("M-#" . define-word)
	 ("C-M-3" . define-word-at-point)
	 ("C-x C-r" . revert-buffer)
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

(defun insert-scripture-ref ()
  (interactive)
  ;; Pure-emacs solution: Look up "completing-read" and build a hash
  ;; table of all scriptures. Here's a link to the docs:
  ;; [[help:completing-read][completing-read]]
  (unless (boundp 'scripture-references-list)
    (load-file "~/.dotfiles/emacs_aux.el"))
  (let ((ref (completing-read "Scripture reference: " scripture-references-list)))
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
