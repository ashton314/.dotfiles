;; Emacs Config file
;; Ashton Wiersdorf

;; Set GC level higher to prevent so many garbage collection cycles
;; during startup. Shaves off 0.2 seconds. Disabled because it might
;; hurt performance later and my startup is already at just 1s
;; (setq gc-cons-threshold 10000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Device-specific customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are variables that point to paths on my filesystem. These are
;; those that need to be changed on a per-install basis
(setq deft-directory "~/Sync/Dropbox/deft")
(setq lsp-java-server-install-dir "~/Sync/repos/java-ls/")
(setq lsp-elixir-server "~/Sync/repos/elixir-ls/release/language_server.sh")
(setq lsp-file-watch-threshold 10000)
(setq org-directory "~/Sync/beorg")
(setq scripture-directory "~/Docs/Scriptures")

;; ITERM2 MOUSE SUPPORT
;; (unless window-system
;;   (require 'mouse)
;;   (xterm-mouse-mode t)
;;   (defun track-mouse (e))
;;   (setq mouse-sel-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-auto-revert-mode)

(setq auto-writer-mode nil)

(setq deft-extensions '("org" "md" "txt" "tex"))
(setq deft-new-file-format "%Y-%m-%d")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)

(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq dabbrev-case-fold-search t)
(menu-bar-mode -1)

(setq tramp-terminal-type "tramp")

;; In case you want emacs to save automatically after n seconds, use:
;; (run-with-timer 10 10 (lambda () (save-some-buffers t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load device-specific variable bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p "~/.emacs.d/device_vars.el")
    (load-file "~/.emacs.d/device_vars.el"))

;; See also at the end of the file for tail-end customizations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Good packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package selectrum-prescient
  :ensure t
  :after 'selectrum)

(use-package selectrum
  :ensure t
  :config
  (setq enable-recursive-minibuffers t)
  (selectrum-mode +1)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; (use-package lsp-java
;;   :after lsp
;;   :config (add-hook 'java-mode-hook 'lsp))

;; Turn off key dimming
(use-package ace-window
  :ensure t
  :config
  (setq aw-background nil)
  (ace-window-display-mode))

(use-package multiple-cursors
  :bind
  (("C-x 4 e" . mc/edit-lines)
   ("C-x 4 >" . mc/mark-next-like-this)
   ("C-x 4 <" . mc/mark-next-previous-this)
   ("C-x 4 d" . mc/mark-all-dwim)))

;; (use-package lastfm
;;   :defer 3)

;; (use-package vuiet
;;   :after lastfm)

;; (use-package lispy
;;   :ensure t)


;; (use-package org-alert
;;   :ensure t
;;   :config
;;   (setq alert-default-style 'osx-notifier)
;;   (setq org-alert-interval 300)           ; Unit: seconds
;;   (org-alert-enable))

(use-package projectile
  :diminish (projectile-mode . " proj")
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))

(use-package avy
  :ensure t
  :bind
  (("C-c j" . avy-goto-line)
   ("C-M-j" . avy-goto-char)
   ("C-c J" . avy-goto-word-0)
   ;; ("C-x j" . avy-goto-char)
   ;; ("C-x J" . avy-goto-char-2)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy-prescient
  :ensure t)

(use-package ivy
  :diminish (ivy-mode . "")
  :ensure t
  :demand
  :bind
  ((:map ivy-mode-map
	 ("C-c '" . ivy-avy))
   (:map ivy-minibuffer-map
         ("C-c >" . ivy-restrict-to-matches)))
  :config
  ;; (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  ;; (setq ivy-count-format "")
  ;; Make the prompt line selectable
  (setq ivy-use-selectable-prompt t)
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; misc
  (setq ivy-use-group-face-if-no-groups t)
  (setq ivy-modified-buffer t)
  (setq ivy-modified-outside-buffer t)

  ;; this has to go here otherwise faces get borked up
  (ivy-prescient-mode)

  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

(use-package counsel
  :ensure t
  :after ivy
  ;; :bind
  ;; (("M-x" . counsel-M-x))
  :config
  (ivy-configure 'counsel-M-x :initial-input ""))

(use-package swiper
  :ensure t
  :after ivy
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper-backward)))

;; Magit
(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status))
  ;; :config
  ;; (setq magit-completing-read-function 'ivy-completing-read)
  )

(use-package magit-todos
  :after (magit)
  :config
  (let ((inhibit-message t))            ; This is to suppress the "not overriding jT keybinding" message
    (magit-todos-mode 1)))

;; (use-package magit-delta
;;   :diminish (magit-delta-mode . " Δ")
;;   :after (magit)
;;   :config
;;   (magit-delta-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (file-exists-p "~/.dotfiles/emacs_aux.el")
    (load-file "~/.dotfiles/emacs_aux.el"))

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
    (shell-command-on-region beg end command (current-buffer) t)
    ))

;;; decrypt region
(defun decrypt-region (beg end)
  "Decrypt a region of a buffer (you will be prompted for a passphrase)"
  (interactive "r")
  (let (passphrase command)
    (setq passphrase (read-passwd "Passphrase: "))
    (setq command (concat "openssl enc -bf -a -d -pass pass:'" passphrase "'"))
    (shell-command-on-region beg end command (current-buffer) t)
    ))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Definitions (key bindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Searching
(define-key global-map (kbd "C-c r") 'counsel-rg)

;; Ace window
(define-key global-map (kbd "M-o") 'ace-window)

;; Overrides
(define-key global-map (kbd "C-x C-r") 'revert-buffer) ; C-x C-r is normally like C-x C-f, but opens file in read-only mode
;; (define-key global-map (kbd "C-r SPC") 'point-to-register) ; Normally C-r is isearch-backwards, but now I use swiper
;; (define-key global-map (kbd "C-r j") 'jump-to-register)

;; Completion
(define-key global-map (kbd "C-x /") 'company-complete)
(define-key global-map (kbd "C-M-_") 'company-ispell)

;; make it so I can use C-p and C-n to move through minibuffer history
(define-key minibuffer-local-map (kbd "C-p") 'previous-line-or-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-line-or-history-element)

;; programming-related
(define-key global-map (kbd "C-c k") 'compile)
(define-key global-map (kbd "C-c (") 'wrap-sexp)
(define-key global-map (kbd "C-c <") 'sp-unwrap-sexp)
(define-key global-map (kbd "C-c )") 'sp-rewrap-sexp)

(define-key global-map (kbd "C-x D") 'dired)
(define-key global-map (kbd "C-x U") 'undo)

;; windowing
(define-key global-map (kbd "C-x O") 'previous-multiframe-window)
(define-key global-map (kbd "C-^") 'enlarge-window)
(define-key global-map (kbd "C-x }") 'sticky-enlarge-window-horizontally)
(define-key global-map (kbd "C-x {") 'sticky-shrink-window-horizontally)
(define-key global-map (kbd "<f7>") 'shrink-window-horizontally)
(define-key global-map (kbd "<f8>") 'balance-windows)
(define-key global-map (kbd "<f9>") 'enlarge-window-horizontally)

;; auto-yasnippets
(define-key global-map (kbd "C-c y w") 'aya-create)
(define-key global-map (kbd "C-c y y") 'aya-expand)
(define-key global-map (kbd "C-c y o") 'aya-open-line)

;; helpers
(define-key global-map (kbd "M-#") 'define-word)
(define-key global-map (kbd "C-x Q") 'query-kill-string)
(define-key global-map (kbd "C-c f n") 'insert-file-name)
(define-key global-map (kbd "C-c s i r") 'string-insert-rectangle)
(define-key global-map (kbd "C-c s r") 'insert-scripture-ref)

;; Registers
(define-key global-map (kbd "C-c C-SPC") 'point-to-register)
(define-key global-map (kbd "C-c C-j") 'jump-to-register)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks and other sweet packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :diminish (company-mode . "")
  :ensure t
  :pin melpa
  :config
  (global-company-mode))

(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode))

(use-package yasnippet
  :diminish (yas-minor-mode . " y")
  :ensure t
  :config
  (yas-global-mode))

(use-package smartparens
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :hook ((elixir-mode . lsp-deferred)
	 (rust-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :bind
  (("C-c C-d" . lsp-describe-thing-at-point))
  :config
  (setq lsp-clients-elixir-server-executable lsp-elixir-server)
  (setq lsp-enable-file-watchers t))


(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (define-key cperl-mode-map (kbd "\C-c t") 'perltidy-dwim)))

(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-global-mode 1)
             (setq dabbrev-case-fold-search nil)))

(add-hook 'company-mode-hook
          '(lambda ()
             (progn
               (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
               (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))))

(add-hook 'elixir-mode-hook
          '(lambda () (progn
                        (define-key elixir-mode-map (kbd "C-M-q") 'elixir-mode-fill-doc-string)
                        ;; (alchemist-mode)
                        ;; (sp-local-pair 'elixir-mode "do\n" "\nend")
                        ;; (poly-alchemist-mode)
                        )))

(add-hook 'scheme-mode-hook 'geiser-mode)
;(setq geiser-default-implementation 'racket)

;(add-hook 'elixir-mode-hook #'lsp-deferred)
;; These two hooks run `elixir-format` on save. Original post: https://github.com/elixir-editors/emacs-elixir#add-elixir-mode-hook-to-run-elixir-format-on-file-save
(add-hook 'elixir-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'elixir-format nil t)))
(add-hook 'elixir-format-hook (lambda ()
                                (setq elixir-format-arguments
                                      (list "--dot-formatter"
                                            (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))))

;(require 'lsp-mode)
;;(require 'lsp-racket)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map (kbd "C-c C-d") 'my-racket-doc)
            (define-key racket-mode-map (kbd "C-c .") 'racket-describe)))

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun org-cycle-list-backwards ()
  (interactive)
  (org-cycle-list-bullet 'previous))

(add-hook 'markdown-mode-hook
	  '(lambda ()
             (progn
               ;; Define these variables so writeroom-mode doesn't freak out
               (setq mouse-wheel-down-event nil)
               (setq mouse-wheel-up-event nil)

               (if auto-writer-mode
                   (writer-mode))

               (require 'org)
               (orgtbl-mode)
	       (define-key markdown-mode-map (kbd "C-c f m") 'open-current-file-macdown)
               (define-key markdown-mode-map [(shift right)] 'org-cycle-list-bullet)
               (define-key markdown-mode-map [(shift left)] 'org-cycle-list-backwards))))

(mapc (lambda (mode)
       (add-hook mode
                 '(lambda () (smartparens-mode))))
     '(prog-mode-hook markdown-mode-hook org-mode-hook))

(mapc (lambda (mode)
       (add-hook mode
                 '(lambda () (progn
                               (sp-pair "`" nil :actions :rem)
                               (sp-pair "'" nil :actions :rem)))))
     '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook racket-mode-hook))

(add-hook 'org-mode-hook
          '(lambda ()
             (require 'org-attach-git)
	     (require 'ox-md nil t)
	     ;; (org-babel-do-load-languages
	     ;;  'org-babel-load-languages
	     ;;  '((emacs-lisp . t)
	     ;;    (elixir . t)
	     ;;    (scheme . t)))
             ))

;; These automatically put hard line-breaks in your text automatically
;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; From the cperl-mode INSTALL.example document from CPAN;
;; Here's what I use to cause cperl-mode to be preferred to perl-mode
;; in Emacs 20.3:
(defun modify-alist (alist-symbol key value &optional search-cdr)
 (let ((alist (symbol-value alist-symbol)))
   (while alist
     (if (eq (if search-cdr
		  (cdr (car alist))
		(car (car alist))) key)
	  (setcdr (car alist) value)
	(setq alist (cdr alist))))))

(modify-alist 'interpreter-mode-alist 'perl-mode 'cperl-mode t)
(modify-alist 'auto-mode-alist        'perl-mode 'cperl-mode t)
(setq cperl-indent-level 4)

(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (define-key cperl-mode-map
	       "\C-c\C-f" 'format-region-68-70)
	     ))

(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (define-key cperl-mode-map
	       (kbd "C-c p") 'perltidy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Poly-mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package polymode
  :ensure t
  :config
  (define-hostmode poly-alchemist-hostmode :mode 'alchemist-mode)
  (define-innermode poly-elixir-doc-innermode
    :mode 'markdown-mode
    :head-matcher "@\\(module\\)?doc *\"\"\""
    :tail-matcher "\"\"\""
    :head-mode 'host
    :tail-mode 'host)
  (define-innermode poly-elixir-template-innermode
    :mode 'web-mode
    :head-matcher "~\\(L\\|E\\)\"\"\""
    :tail-matcher "\"\"\""
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-alchemist-mode
    :hostmode 'poly-alchemist-hostmode
    :innermodes '(poly-elixir-doc-innermode poly-elixir-template-innermode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode customizations (org mode customizations)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-return--around (old-fn &rest args)
  (let ((context (org-element-lineage (org-element-at-point) '(item))))
    (if (and context (not args))
        (org-insert-item (org-element-property :checkbox context))
      (apply old-fn args))))

(advice-add 'org-return :around 'org-return--around)

;; (defun scripture-ref (ref)
;;   "Given a standardized scripture reference, return the last part of the URL to access this online."
;;   "bofm/alma/1")

;; This was in custom-set-variables
;     '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
;; Put it back to turn on org-trello niceties
;; This showed up later:
;  '(org-trello-current-prefix-keybinding "C-c o")

(setq org-default-notes-file (concat org-directory "/mobile_inbox.org"))
(setq org-family-notes-file (concat org-directory "/family_shared.org"))
(setq org-general-notes-file (concat org-directory "/general.org"))
(setq org-bishopric-file (concat org-directory "/bishopric.org"))
(setq org-ward-council-file (concat org-directory "/ward_council.org"))
(setq org-project-notes-file (concat org-directory "/projects.org"))
(setq org-work-notes-file (concat org-directory "/work.org"))
(setq org-notes-file (concat org-directory "/notes.org"))
;(setq org-20bn-file (concat org-directory "/20bn.org"))
(setq org-school-file (concat org-directory "/school.org"))
(setq org-for-later-file (concat org-directory "/for_later.org"))
(setq org-research-directory "~/Sync/Dropbox/undergrad_research/research-notes")
(setq org-research-tasks (concat org-research-directory "/research_tasks.org"))
(setq org-scripture-study-file "~/Sync/Dropbox/study_journal/HEAD.org")

(setq org-log-done 'time)               ; Instead of `'time`, also try `'note`
(setq org-log-into-drawer t)            ; Move log notes into a drawer

(setq org-tag-alist '(
                      ;; locale
                      (:startgroup)
                      ("church")
                      ("home" . ?h)
                      ("work" . ?w)
                      ("school" . ?s)
                      ("research" . ?r)
                      (:endgroup)
                      (:newline)
                      ;; context
                      ("computer" . ?c)
                      ("phone" . ?p)
                      ("emacs" . ?e)
                      ("mail" . ?m)
                      (:newline)
                      ;; scale
                      (:startgroup)
                      ("project" . ?j)
                      ("tiny" . ?t)
                      (:endgroup)
                      ;; misc
                      ("meta")
                      ("review")
                      ))

(setq org-todo-keywords
      '((sequence "TODO(t)" "BLOCKED(b@)" "IN_PROGRESS(p!)" "|" "DONE(d!)" "WONT_FIX(w@)")))

(setq org-capture-templates
      '(("t" "General Todo" entry (file org-default-notes-file)
	 "* TODO %?\n   %U\n%i\n%a")
        ("s" "Scripture Study" entry (file+headline org-scripture-study-file "HEAD")
         "** %?\n    %U")
        ("c" "Computer Science")
        ("r" "Religion Classes")
        ("p" "Programming Language Research")
	("g" "General Homework" entry (file+headline org-school-file "General")
	 "** TODO %?\n   %U\n%i\n%a")
        ;; CS
        ("cs" "CS 324 (Systems Programming)" entry (file+headline org-school-file "CS 324 (Systems)")
         "** TODO %? :homework:cs_324:\n %U\n %i\n %a")
        ("cd" "CS 340 (Software Design)" entry (file+headline org-school-file "CS 340 (Software Design)")
         "** TODO %? :homework:cs_340:\n %U\n %i\n %a")
        ;; Religion
        ("rp" "REL A 327 (Pearl of Great Price)" entry (file+headline org-school-file "REL A 327 (Pearl of Great Price)")
         "** TODO %? :homework:rel_327:\n %U\n %i\n %a")
        ("ri" "REL A 304 (Writings of Isaiah)" entry (file+headline org-school-file "REL A 304 (Writings of Isaiah)")
         "** TODO %? :homework:rel_304:\n %U\n %i\n %a")
        ;; PL Research
        ("pt" "Research Task" entry (file org-research-tasks)
         "* TODO %?\n  %U\n %i\n %a")
        ;; ("pn" "Research Note" entry ;; FIXME: add file
        ;;  "** %?\n   %U\n %i\n %a")
	))

(setq org-agenda-custom-commands
      '(("n" "Agenda and All Todos"
	 ((agenda)
	  (todo)))
	("g" "Gospel Study"
	 ((agenda ((org-agenda-files '("~/Personal/study_journal/HEAD.org"))))
	  (tags-todo "gospel")))))

;; Links
(define-key global-map (kbd "C-c o l") 'org-store-link)
(define-key global-map (kbd "C-c L") 'org-insert-link-global)
(define-key global-map (kbd "C-c O") 'org-open-at-point-global)

;; Other org-related keys
(define-key global-map (kbd "C-c o c") 'org-capture)
(define-key global-map (kbd "C-c o a") 'org-agenda)
(define-key global-map (kbd "C-c o b") 'org-switchb)
(define-key global-map (kbd "C-c o '") 'org-cycle-agenda-files)
(define-key global-map (kbd "C-c o s") 'org-save-all-org-buffers)
(define-key global-map (kbd "C-c C,") 'org-insert-structure-template)
(define-key global-map (kbd "C-c C>") 'org-demote-subtree)
(define-key global-map (kbd "C-c C<") 'org-promote-subtree)
(setq org-link-abbrev-alist
      `(("scrip" . ,(concat "file:" scripture-directory "/lds_scriptures.txt::<<%s>>"))
;;      '(("scrip" . "https://www.lds.org/languages/eng/content/scriptures/%(scripture-ref)")
	("famhist/person" . "https://www.familysearch.org/tree/person/details/%s")))

(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets
      ;; `((org-agenda-files . (:level . 1))
      `(((,org-project-notes-file) . (:level . 0))
        ((,org-notes-file) . (:level . 0))
        ((,org-general-notes-file) . (:level . 1))
        ((,org-for-later-file) . (:level . 0))
        ((,org-family-notes-file) . (:maxlevel . 1))
        ((,org-school-file) . (:maxlevel . 1))))
;; (setq org-refile-targets
;;       ;; `((org-agenda-files . (:level . 1))
;;       `(((,org-project-notes-file) . (:level . 0))
;;         ((,org-notes-file) . (:level . 0))
;;         ((,org-general-notes-file) . (:level . 0))
;;         ((,org-family-notes-file) . (:maxlevel . 1))
;;         ((,org-school-file) . (:maxlevel . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display/mode line customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display-time)
(setq column-number-mode t)
(setq line-number-mode t)
(setq tool-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.2)
 '(company-show-numbers t)
 '(counsel-projectile-mode t nil (counsel-projectile))
 '(counsel-rg-base-command
   "rg -M 200 --with-filename --no-heading --line-number --color never %s")
 '(deft-auto-save-interval 30.0)
 '(dired-use-ls-dired nil)
 '(find-file-visit-truename t)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message
   ";; This space intentionally left blank. Try \\[find-file].

")
 '(ispell-query-replace-choices t)
 '(lsp-ui-sideline-enable nil)
 '(olivetti-body-width 80)
 '(org-agenda-files
   '("~/Sync/beorg/mobile_inbox.org" "~/Sync/beorg/general.org" "~/Sync/Dropbox/beorg/for_later.org" "~/Sync/Dropbox/undergrad_research/research-notes/research_tasks.org" "~/Sync/beorg/school.org" "~/Sync/beorg/family_shared.org" "~/Sync/beorg/projects.org" "~/Sync/beorg/work.org"))
 '(org-fontify-quote-and-verse-blocks t)
 '(org-ref-insert-link-function 'org-ref-helm-insert-cite-link)
 '(org-tags-column -100)
 '(package-selected-packages
   '(multiple-cursors magit-delta wgrep magit-todos kotlin-mode company-prescient minimap counsel-projectile lsp-java projectile json-mode ivy-prescient flx counsel diminish org-pomodoro number nov org bind-key use-package markdown-mode+ poly-markdown esup bbdb ioccur csv-mode alert org-alert edit-indirect magit ace-window htmlize keyfreq company-lsp lsp-elixir poly-org imenu-list olivetti elixir-yasnippets haskell-snippets auto-yasnippet centered-cursor-mode writeroom-mode pcre2el company-web flycheck-mix smartparens julia-mode racket-mode free-keys swiper swift-mode haskell-mode toml-mode define-word pandoc pandoc-mode clojure-mode clojure-mode-extra-font-locking lorem-ipsum yaml-mode darkroom cargo racer rust-mode rust-playground web-mode elixir-mode ob-elixir erlang dockerfile-mode perl6-mode sos deft))
 '(safe-local-variable-values '((org-tags-column . -150)))
 '(scheme-program-name "racket")
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(tab-bar-show nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face-0 ((t (:background "grey30" :foreground "white"))))
 '(avy-lead-face-2 ((t (:background "grey55" :foreground "white"))))
 '(cider-debug-code-overlay-face ((t (:background "color-238"))))
 '(company-tooltip-search ((t (:inherit highlight :background "#1096ff" :foreground "brightwhite"))))
 '(company-tooltip-search-selection ((t (:inherit company-tooltip-search))))
 '(cperl-array-face ((t (:foreground "cyan" :underline t :weight bold))))
 '(cperl-hash-face ((t (:foreground "magenta" :underline t :slant normal :weight bold))))
 '(custom-state ((t (:foreground "#00ff70"))))
 '(custom-variable-tag ((t (:foreground "color-33" :weight bold))))
 '(ediff-current-diff-A ((t (:background "#aa0000"))))
 '(ediff-current-diff-Ancestor ((t (:background "#bb9aaa"))))
 '(ediff-current-diff-B ((t (:background "#00bb00" :foreground "color-255"))))
 '(ediff-current-diff-C ((t (:background "#999900"))))
 '(ediff-even-diff-A ((t (:background "#0000ff"))))
 '(ediff-even-diff-B ((t (:background "magenta"))))
 '(ediff-fine-diff-A ((t (:background "#ee5656"))))
 '(ediff-odd-diff-A ((t (:background "green"))))
 '(ediff-odd-diff-B ((t (:background "red"))))
 '(elixir-atom-face ((t (:foreground "RoyalBlue3"))))
 '(font-lock-builtin-face ((t (:foreground "#74758c"))))
 '(font-lock-comment-face ((t (:foreground "orangered"))))
 '(font-lock-function-name-face ((t (:foreground "#0087ff"))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(geiser-font-lock-autodoc-identifier ((t (:foreground "color-27"))))
 '(git-commit-summary ((t (:foreground "green"))))
 '(helm-buffer-directory ((t (:foreground "blue" :underline t))))
 '(helm-buffer-file ((t (:foreground "#909090"))))
 '(helm-buffer-saved-out ((t (:foreground "orange"))))
 '(helm-selection ((t (:background "#333" :distant-foreground "black"))))
 '(helm-selection-line ((t (:background "#333" :distant-foreground "black"))))
 '(helm-source-header ((t (:background "#abd7f0" :foreground "#000" :weight bold :height 1.3 :family "Sans Serif"))))
 '(highlight ((t (:background "#002010"))))
 '(imenu-list-entry-face-0 ((t (:inherit imenu-list-entry-face :foreground "color-165"))))
 '(imenu-list-entry-face-1 ((t (:inherit imenu-list-entry-face :foreground "color-40"))))
 '(imenu-list-entry-face-2 ((t (:inherit imenu-list-entry-face :foreground "color-33"))))
 '(ioccur-overlay-face ((t (:background "color-22" :underline t))))
 '(isearch ((t (:background "magenta3" :foreground "black"))))
 '(isearch-fail ((t (:background "RosyBrown1" :foreground "black"))))
 '(italic ((t (:foreground "#ffa500" :underline nil :slant italic))))
 '(ivy-current-match ((t (:background "#1a4bf7" :foreground "white"))))
 '(ivy-minibuffer-match-face-2 ((t (:foreground "green3" :underline t :weight bold))))
 '(link ((t (:foreground "#1e90ff" :underline t))))
 '(link-visited ((t (:inherit link :foreground "magenta2"))))
 '(lsp-lsp-flycheck-warning-unnecessary-face ((t (:background "color-238" :slant italic))) t)
 '(magit-diff-added ((t (:background "#003000"))))
 '(magit-diff-added-highlight ((t (:background "#005000"))))
 '(magit-diff-base ((t (:background "#ffffcc" :foreground "#777711"))))
 '(magit-diff-base-highlight ((t (:background "#777711" :foreground "#ffffaa"))))
 '(magit-diff-context ((t (:foreground "grey80"))))
 '(magit-diff-context-highlight ((t (:background "grey25"))))
 '(magit-diff-hunk-heading ((t (:background "grey80" :foreground "black"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "grey70" :foreground "grey10"))))
 '(magit-diff-hunk-heading-selection ((t (:inherit magit-diff-hunk-heading-highlight :foreground "salmon4"))))
 '(magit-diff-removed ((t (:background "#300000"))))
 '(magit-diff-removed-highlight ((t (:background "#500000"))))
 '(magit-section-highlight ((t (:background "color-233"))))
 '(markdown-bold-face ((t (:foreground "color-208" :slant normal :weight bold))))
 '(markdown-header-face ((t (:foreground "blue" :weight bold))))
 '(markdown-header-face-1 ((t (:foreground "deepskyblue1" :weight bold :height 1.0))))
 '(markdown-header-face-2 ((t (:foreground "steelblue1" :weight bold :height 1.0))))
 '(markdown-header-face-3 ((t (:foreground "dodgerblue1" :weight bold :height 1.0))))
 '(markdown-header-face-4 ((t (:foreground "deepskyblue3" :weight normal :height 1.0))))
 '(markdown-header-face-5 ((t (:foreground "steelblue3" :height 1.0))))
 '(markdown-header-face-6 ((t (:foreground "dodgerblue3" :height 1.0))))
 '(match ((t (:background "yellow3" :foreground "black"))))
 '(minibuffer-prompt ((t (:foreground "#00bfff"))))
 '(mode-line ((t (:background "dodgerblue4" :foreground "grey96" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey70" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light))))
 '(org-agenda-structure ((t (:foreground "#0087ff"))))
 '(org-babel-load-languages '(emacs-lisp elixir))
 '(org-document-info ((t (:foreground "deepskyblue1"))))
 '(org-document-title ((t (:foreground "turquoise" :weight bold))))
 '(org-drawer ((t (:foreground "steelblue1"))))
 '(org-quote ((t (:foreground "green" :slant italic))))
 '(org-scheduled-today ((t (:foreground "limegreen"))))
 '(org-table ((t (:foreground "#4F9EFF"))))
 '(org-todo ((t (:foreground "#ee3030" :weight bold))))
 '(org-upcoming-deadline ((t (:foreground "orange"))))
 '(org-verbatim ((t (:foreground "#7cfc00"))))
 '(region ((t (:background "#000087"))))
 '(secondary-selection ((t (:background "yellow1" :foreground "black"))))
 '(selectrum-current-candidate ((t (:background "#0030ff"))))
 '(selectrum-primary-highlight ((t (:foreground "brightgreen" :underline t))))
 '(selectrum-secondary-highlight ((t (:underline t :weight bold))))
 '(shadow ((t (:foreground "slategray"))))
 '(show-paren-match ((t (:background "#5aa"))))
 '(show-paren-match-expression ((t (:background "#232323"))))
 '(smerge-lower ((t (:background "#003000"))))
 '(smerge-markers ((t (:background "#009090" :foreground "brightwhite"))))
 '(smerge-mine ((t (:background "#0f0d0d"))) t)
 '(smerge-other ((t (:background "#0d2f0d"))) t)
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#005000"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "#550000"))))
 '(smerge-upper ((t (:background "#350000"))))
 '(sp-pair-overlay-face ((t (:background "color-236"))))
 '(sp-show-pair-match-content-face ((t (:background "#666"))) t)
 '(web-mode-html-tag-bracket-face ((t (:foreground "brightwhite"))))
 '(web-mode-html-tag-face ((t (:foreground "brightblue"))))
 '(widget-field ((t (:background "yellow3" :foreground "black")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Load device-specific customizations
(if (file-exists-p "~/.emacs.d/device_local.el")
    (load-file "~/.emacs.d/device_local.el"))
