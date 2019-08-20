;; Emacs Config file
;; Ashton Wiersdorf

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
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn off key dimming
(setq aw-background nil)
(ace-window-display-mode)

;(require 'org-alert)
;(setq alert-default-style 'osx-notifier)
;(setq org-alert-interval 1200)           ; Unit: seconds
;(org-alert-enable)

(setq auto-writer-mode nil)

(setq deft-directory "~/Sync/Box Sync/deft/")
(setq deft-extensions '("md" "org" "txt" "tex"))
(setq deft-new-file-format "%Y-%m-%d")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)

(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq dabbrev-case-fold-search t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's take this mode for a spin!
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ;; TODO: Look up "completing-read" and build a hash table of all
  ;; scriptures. Here's a link to the docs:
  ;; [[help:completing-read][completing-read]]
  (let ((ref (read-from-minibuffer "Scripture Reference: ")))
    (insert (format "[[scrip:%s][%s]]" ref ref))))

(defun toggle-auto-writer-mode ()
  (interactive)
  (setq auto-writer-mode (not auto-writer-mode))
  (display-message-or-buffer (format "Auto writer mode set to %s" auto-writer-mode)))

(defun writer-mode ()
  (interactive)
  (progn
    (writeroom-mode)
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
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%F")
                   ((equal prefix '(4)) "%F %T")
                   ((equal prefix '(16)) "%A, %e %B %Y")))
          (system-time-locale))
          ;; (system-time-locale "de_DE"))
      (insert (format-time-string format))))

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
  (cond ((not prefix) (racket-describe))
        ((equal prefix '(4)) (racket-doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Definitions (key bindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Searching
(define-key global-map (kbd "C-h C-s") 'helm-ag)

;; Magit
(define-key global-map (kbd "C-x g") 'magit-status)

;; Ace window
(define-key global-map (kbd "M-o") 'ace-window)

;; Overrides
(define-key global-map (kbd "C-x C-r") 'revert-buffer) ; C-x C-r is normally like C-x C-f, but opens file in read-only mode

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
;(define-key global-map (kbd "C-x f") 'forward-sexp)
;(define-key global-map (kbd "C-x B") 'backward-sexp)
;(define-key global-map (kbd "C-x d") 'down-list)
;(define-key global-map (kbd "C-x u") 'backward-up-list)

(define-key global-map (kbd "C-x D") 'dired)
(define-key global-map (kbd "C-x U") 'undo)

;; Special modes

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

;; (autoload 'gfm-mode "gfm-mode"
;;   "Major mode for editing GitHub Flavored Markdown files" t)
;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; get scroll (and mouse?) support in iTerm2
;; (require 'mwheel)
;; (require 'mouse)
;; (xterm-mouse-mode t)
;; (mouse-wheel-mode t)
;; (global-set-key [mouse-4] 'next-line)
;; (global-set-key [mouse-5] 'previous-line)
;; (setq mouse-wheel-progressive-speed nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook '(lambda ()
                              (global-company-mode)
                              (require 'yasnippet)
;			      (keyfreq-mode 1)
;			      (keyfreq-autosave-mode 1)
                              ))

(add-hook 'lsp-mode-hook
          '(lambda ()
             (define-key lsp-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)))

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
                        (define-key elixir-mode-map (kbd "M-q") 'elixir-mode-fill-doc-string)
                        ;; (alchemist-mode)
                        ;; (sp-local-pair 'elixir-mode "do\n" "\nend")
                        ;; (poly-alchemist-mode)
                        )))

(add-hook 'scheme-mode-hook 'geiser-mode)
;(setq geiser-default-implementation 'racket)

(add-hook 'elixir-mode-hook #'lsp-deferred)
;; These two hooks run `elixir-format` on save. Original post: https://github.com/elixir-editors/emacs-elixir#add-elixir-mode-hook-to-run-elixir-format-on-file-save
;; (add-hook 'elixir-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'elixir-format nil t)))
;; (add-hook 'elixir-format-hook (lambda ()
;;                                 (setq elixir-format-arguments
;;                                       (list "--dot-formatter"
;;                                             (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))))

;(require 'lsp-mode)
;;(require 'lsp-racket)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map (kbd "C-c C-d") 'my-racket-doc)
            (define-key racket-mode-map (kbd "C-c .") 'racket-describe)))

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; (use-package lsp-mode
;;              :hook (elixir-mode . lsp-deferred)
;;              :commands (lsp lsp-deferred))

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
          '(lambda () (org-babel-do-load-languages
                       'org-babel-load-languages
                       '((emacs-lisp . t)
                         (elixir . t)
                         (scheme . t)))))

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
  :innermodes '(poly-elixir-doc-innermode poly-elixir-template-innermode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun scripture-ref (ref)
;;   "Given a standardized scripture reference, return the last part of the URL to access this online."
;;   "bofm/alma/1")

;; This was in custom-set-variables
;     '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
;; Put it back to turn on org-trello niceties
;; This showed up later:
;  '(org-trello-current-prefix-keybinding "C-c o")


(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "SCHEDULED" "IN_PROGRESS" "|" "DONE" "REFILED")))

(global-set-key (kbd "C-c L") 'org-insert-link-global) 
(global-set-key (kbd "C-c O") 'org-open-at-point-global)

(setq org-directory "~/Sync/Dropbox/beorg")
(setq org-default-notes-file (concat org-directory "/mobile_inbox.org"))
(setq org-general-notes-file (concat org-directory "/general.org"))
(setq org-bishopric-file (concat org-directory "/bishopric.org"))
(setq org-ward-council-file (concat org-directory "/ward_council.org"))
(setq org-project-notes-file (concat org-directory "/projects.org"))
(setq org-work-notes-file (concat org-directory "/work.org"))
(define-key global-map (kbd "C-c o c") 'org-capture)
(define-key global-map (kbd "C-c o a") 'org-agenda)
(define-key global-map (kbd "C-c o l") 'org-store-link)
(define-key global-map (kbd "C-c o b") 'org-switchb)
(define-key global-map (kbd "C-c o s") 'org-save-all-org-buffers)
(setq org-link-abbrev-alist
      '(("scrip" . "file:~/Docs/Scriptures/lds_scriptures.txt::<<%s>>")
;;      '(("scrip" . "https://www.lds.org/languages/eng/content/scriptures/%(scripture-ref)")
	("famhist/person" . "https://www.familysearch.org/tree/person/details/%s")))

(setq org-refile-targets
      ;; `((org-agenda-files . (:level . 1))
      `(((,org-bishopric-file) . (:maxlevel . 2))
        ((,org-project-notes-file) . (:level . 1))
        ((,org-general-notes-file) . (:maxlevel . 2))
        ((,org-work-notes-file) . (:maxlevel . 2))))
      
      ;; '((org-agenda-files . (:level . 1))
      ;;   (org-default-notes-file . (:level . 1))
      ;;   (org-project-notes-file . (:maxlevel . 2))))

;; ghost-blog customizations
(setq ghost-blog-url "https://ashton.wiersdorf.org/ghost/api/v0.1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display customizations
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
 '(deft-auto-save-interval 30.0)
 '(dired-listing-switches "-avlF")
 '(initial-major-mode (quote text-mode))
 '(lsp-clients-elixir-server-executable "~/Sync/repos/elixir-ls/release/language_server.sh")
 '(olivetti-body-width 80)
 '(org-agenda-files
   (quote
    ("~/Sync/Dropbox/beorg/projects.org" "~/Sync/Dropbox/beorg/research.org" "~/Sync/Dropbox/beorg/work.org" "~/Sync/Dropbox/beorg/mobile_inbox.org" "~/Sync/Dropbox/beorg/general.org" "~/Sync/Dropbox/beorg/bishopric.org" "~/Sync/Dropbox/beorg/ward_council.org" "~/Personal/study_journal/Family_Counsel.org" "~/Personal/study_journal/HEAD.org")))
 '(package-selected-packages
   (quote
    (markdown-mode+ poly-markdown esup bbdb ioccur csv-mode alert org-alert helm-ag edit-indirect magit org-ref ace-window htmlize keyfreq company-lsp lsp-elixir helm-swoop poly-org imenu-list olivetti elixir-yasnippets haskell-snippets auto-yasnippet centered-cursor-mode writeroom-mode pcre2el company-web flycheck-mix smartparens julia-mode racket-mode free-keys swiper swift-mode ac-cider haskell-mode bm toml-mode define-word helm-wordnet pandoc pandoc-mode clojure-cheatsheet clojure-mode clojure-mode-extra-font-locking lorem-ipsum bison-mode yaml-mode sublimity darkroom ox-gfm cargo racer rust-mode rust-playground web-mode org-trello alchemist elixir-mode ob-elixir visual-fill-column erlang dockerfile-mode perl6-mode sos geiser quack slime deft)))
 '(scheme-program-name "racket")
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style (quote expression)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-debug-code-overlay-face ((t (:background "color-238"))))
 '(cperl-array-face ((t (:foreground "cyan" :underline t :weight bold))))
 '(cperl-hash-face ((t (:foreground "magenta" :underline t :slant normal :weight bold))))
 '(ediff-current-diff-B ((t (:background "#56bb56" :foreground "color-255"))))
 '(ediff-even-diff-A ((t (:background "blue"))))
 '(ediff-even-diff-B ((t (:background "magenta"))))
 '(ediff-fine-diff-A ((t (:background "#ee5656"))))
 '(ediff-odd-diff-A ((t (:background "green"))))
 '(ediff-odd-diff-B ((t (:background "red"))))
 '(elixir-atom-face ((t (:foreground "RoyalBlue3"))))
 '(font-lock-builtin-face ((t (:foreground "#74758c"))))
 '(font-lock-comment-face ((t (:foreground "#f22"))))
 '(font-lock-function-name-face ((t (:foreground "color-33"))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(geiser-font-lock-autodoc-identifier ((t (:foreground "color-27"))))
 '(helm-buffer-directory ((t (:foreground "blue" :underline t))))
 '(helm-buffer-file ((t (:foreground "#909090"))))
 '(helm-buffer-saved-out ((t (:foreground "orange"))))
 '(helm-selection ((t (:background "#333" :distant-foreground "black"))))
 '(helm-selection-line ((t (:background "#333" :distant-foreground "black"))))
 '(highlight ((t (:background "#020"))))
 '(imenu-list-entry-face-0 ((t (:inherit imenu-list-entry-face :foreground "color-165"))))
 '(imenu-list-entry-face-1 ((t (:inherit imenu-list-entry-face :foreground "color-40"))))
 '(imenu-list-entry-face-2 ((t (:inherit imenu-list-entry-face :foreground "color-33"))))
 '(ioccur-overlay-face ((t (:background "color-22" :underline t))))
 '(isearch ((t (:background "magenta3" :foreground "black"))))
 '(isearch-fail ((t (:background "RosyBrown1" :foreground "black"))))
 '(magit-diff-added-highlight ((t (:background "#000500"))))
 '(magit-diff-context-highlight ((t (:background "grey25"))))
 '(magit-diff-removed-highlight ((t (:background "#100"))))
 '(magit-section-highlight ((t (:background "grey20"))))
 '(markdown-bold-face ((t (:foreground "color-208" :slant normal :weight bold))))
 '(markdown-header-face ((t (:foreground "blue" :weight bold))))
 '(markdown-header-face-1 ((t (:foreground "#55ffff" :weight bold :height 1.0))))
 '(markdown-header-face-2 ((t (:foreground "color-45" :weight bold :height 1.0))))
 '(markdown-header-face-3 ((t (:foreground "color-39" :weight bold :height 1.0))))
 '(markdown-header-face-4 ((t (:foreground "color-33" :weight normal :height 1.0))))
 '(markdown-header-face-5 ((t (:foreground "color-27" :height 1.0))))
 '(markdown-header-face-6 ((t (:foreground "color-21" :height 1.0))))
 '(match ((t (:background "yellow3" :foreground "black"))))
 '(minibuffer-prompt ((t (:foreground "brightblue"))))
 '(mode-line ((t (:background "grey90" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey70" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light))))
 '(org-babel-load-languages (quote (emacs-lisp elixir)))
 '(org-document-info ((t (:foreground "blue"))))
 '(org-document-title ((t (:foreground "blue" :weight bold))))
 '(org-quote ((t (:foreground "green"))))
 '(org-table ((t (:foreground "#4F9EFF"))))
 '(org-verbatim ((t (:foreground "color-34"))))
 '(region ((t (:background "color-18"))))
 '(secondary-selection ((t (:background "yellow1" :foreground "black"))))
 '(show-paren-match ((t (:background "#5aa"))))
 '(show-paren-match-expression ((t (:background "#232323"))))
 '(smerge-lower ((t (:background "#0d2f0d"))))
 '(smerge-markers ((t (:background "color-237"))))
 '(smerge-mine ((t (:background "#0f0d0d"))) t)
 '(smerge-other ((t (:background "#0d2f0d"))) t)
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#0a0f0a"))))
 '(smerge-upper ((t (:background "#0f0d0d"))))
 '(sp-pair-overlay-face ((t (:background "color-236"))))
 '(sp-show-pair-match-content-face ((t (:background "#666"))) t)
 '(web-mode-html-tag-bracket-face ((t (:foreground "brightwhite"))))
 '(web-mode-html-tag-face ((t (:foreground "brightblue")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
