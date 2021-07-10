;; Core packages and whatnot

(display-time)
(setq column-number-mode t)
(setq line-number-mode t)
(global-auto-revert-mode)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq bidi-inhibit-bpa t)
(setq frame-resize-pixelwise t)
(setq default-frame-alist '((width . 87) (height . 60)))
(setq inhibit-startup-screen t)

(set-fontset-font
 t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

(defvar evil-auto-save-buffer-modes '(text-mode scribble-mode org-mode markdown-mode racket-mode emacs-lisp-mode rust-mode))

;; Theme
(use-package nord-theme
  :config
  (load-theme 'nord t))

(use-package diminish)
(diminish 'eldoc-mode "")

(use-package emacs
  ;; Markdown mode is built-in now I guess
  :hook ((markdown-mode . visual-line-mode)))

(use-package org
  :defer t
  :hook ((org-mode . visual-line-mode))
  :bind (:map global-map
	      ("C-c o c" . org-capture)
	      ("C-c o a" . org-agenda)
	      ("C-c o '" . org-cycle-agenda-files)
	      ("C-c o l" . org-store-link)
	      ("C-c L" . org-insert-link-global)
	      ("C-c O" . org-open-at-point-global))
  :config
  (unless (boundp 'scripture-directory) (setq scripture-directory ""))
  (setq org-export-with-smart-quotes t)
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-family-notes-file (concat org-directory "/family_shared.org"))
  (setq org-general-tasks-file (concat org-directory "/general.org"))
  (setq org-ward-council-file (concat org-directory "/ward_council.org"))
  (setq org-project-notes-file (concat org-directory "/projects.org"))
  (setq org-work-notes-file (concat org-directory "/work.org"))
  (setq org-notes-file (concat org-directory "/notes.org"))
  (setq org-school-file (concat org-directory "/school.org"))
  (setq org-for-later-file (concat org-directory "/for_later.org"))
  (setq org-vocab-file (concat org-directory "/vocab.org"))
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
			("group" . ?g)
			("project" . ?j)
			("tiny" . ?t)
			(:endgroup)
			;; misc
			("borrowed_item")	; reminders to return or follow-up on things
			("meta")
			("review")
			("german")
			("reading")
			))

  (setq org-todo-keywords
	'((sequence "TODO(t)" "BLOCKED(b@)" "IN_PROGRESS(p!)" "|" "DONE(d!)" "WONT_FIX(w@)")))

  (setq org-capture-templates
	'(("c" "Quick Capture" entry (file org-default-notes-file)
	   "* TODO %?\n   %U\n%i\n%a")
	  ("t" "General TODO Item" entry (file org-general-tasks-file)
	   "* TODO %?\n   %U\n%i\n%a")
	  ("n" "Note for Later" entry (file org-for-later-file)
	   "* %?\n    %U\n%i\n%a")
	  ("w" "Vocab word" entry (file org-vocab-file)
	   "* %?\n  %U\n%i")))

  (setq org-agenda-custom-commands
	'(("n" "Agenda and All Todos"
	   ((agenda)
	    (todo)))
	  ("g" "Gospel Study"
	   ((agenda ((org-agenda-files '("~/Personal/study_journal/HEAD.org"))))
	    (tags-todo "gospel")))))

  (setq org-link-abbrev-alist
	`(("scrip" . ,(concat "file:" scripture-directory "/lds_scriptures.txt::<<%s>>"))
	  ("famhist/person" . "https://www.familysearch.org/tree/person/details/%s")))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets
	`(((,org-for-later-file) . (:level . 0))
	  ((,org-for-later-file) . (:level . 1))
	  ((,org-general-tasks-file) . (:level . 0))
	  ((,org-general-tasks-file) . (:level . 1))
	  ((,org-notes-file) . (:level . 0))
	  ((,org-notes-file) . (:level . 1))
	  ((,org-school-file) . (:level . 1)))))

;; Code
(use-package projectile
  :diminish " p"
  :init
  (projectile-mode +1)
  :bind (("C-x p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/spiff/" "~/.dotfiles/")))

(use-package racket-mode
  :defer t)

(use-package scribble-mode
  :mode "\\.scrbl\\'"
  :config
  (add-hook 'scribble-mode-hook #'configure-scribble-mode))

(use-package vterm
  :defer t)

(use-package paredit
  :defer t
  :diminish "(λ)"
  :hook ((racket-mode lisp-mode emacs-lisp-mode) . paredit-mode))

(use-package smartparens
  :defer t
  :diminish "(s)"
  :hook ((elixir-mode rust-mode) . smartparens-mode))

(use-package yasnippet
  :diminish ""
  :bind (("M-<tab>" . 'yas-expand))
  :config
  (yas-global-mode +1))

(use-package auto-yasnippet
  :diminish "")

;; Completion

(use-package vertico
  :defer t
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(initials orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package savehist
  :defer t
  :init
  (savehist-mode))

(use-package corfu
  :config
  (corfu-global-mode))

(defun yas-expand-or-complete (&optional arg)
  "Try to expand a yasnippet. If none available at point, run company-indent-or-complete."
  (interactive)
  (or (yas-expand) (company-indent-or-complete-common arg)))

(use-package company
  :defer t
  :diminish ""
  :hook ((prog-mode) . (lambda () (corfu-mode -1) (company-mode)))
  :config
  (setq tab-always-indent 'complete)
  (setq company-idle-delay nil)
  (setq company-minimum-prefix-length 3)
  ;; For this, see https://github.com/jojojames/vscode-icon-emacs
  ;; (setq company-format-margin-function #'company-vscode-light-icons-margin)
  :bind (:map company-mode-map
	      ("<tab>" . 'yas-expand-or-complete)
	      :map company-active-map
	      ("C-n" . 'company-select-next-or-abort)
	      ("C-j" . 'company-select-next-or-abort)
	      ("C-p" . 'company-select-previous-or-abort)
	      ("C-k" . 'company-select-previous-or-abort)))

(use-package company-prescient
  :defer t
  :hook (company-mode . company-prescient-mode))

(use-package dabbrev
  :defer t
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package emacs
  :defer t
  :init
  (setq enable-recursive-minibuffers t)
  (setq completion-cycle-threshold 1)
  (setq tab-always-indent 'complete))

(defun my-consult-rg (&optional dir initial)
  "Same as `consult-ripgrep', but sets `selectrum-group-format' to `nil'."
  (interactive "P")
  (let ((selectrum-group-format nil))
    (consult-ripgrep dir initial)))

(defun maybe-save-buffer ()
  "Save buffer if major mode is listed in `evil-auto-save-buffer-modes'."
  (when (and (member major-mode evil-auto-save-buffer-modes)
	     (buffer-file-name))
    (save-buffer)))

(use-package consult
  :defer t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (("C-x M-:" . consult-complex-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-keep-lines)
         ("C-c C-k" . consult-flush-lines)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)       ;; "M-s o" is a good alternative.
         ("C-s"   . consult-line)          ;; "M-s l" is a good alternative.
         ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
         ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
         ("M-g r" . consult-git-grep)      ;; or consult-grep, consult-ripgrep
	 ("s-r r" . my-consult-rg)
         ("M-g f" . consult-find)          ;; or consult-locate, my-fdfind
         ("M-g i" . consult-project-imenu) ;; or consult-imenu
         ("M-g e" . consult-error)
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))

  :init
  (defun my-fdfind (&optional dir)
    (interactive "P")
    (let ((consult-find-command '("fdfind" "--color=never" "--full-path")))
      (consult-find dir)))
  (fset 'multi-occur #'consult-multi-occur)
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (setq consult-narrow-key "<") ;; Another viable option: (kbd "C-+")
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package marginalia
  :config
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package embark
  :bind
  (("C-S-a" . embark-act)	; ctrl-shift-a
   ("C-s-a" . embark-act)	; ctrl-super-a
   ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

;; Evil

(use-package undo-fu
  :defer t)

(use-package origami
  :defer t)

(use-package evil-leader
  :defer t
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ;; Misc. commands
    "x" 'execute-extended-command
    "/" 'company-complete
    "t" 'tab-new
    "wk" 'which-key-show-major-mode
    "fo" 'other-frame

    ;; Evil-Nerd-Commenter
    "cc" 'evilnc-comment-or-uncomment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "ct" 'add-todo-comment
    
    ;; Consult commands
    "m"  'consult-bookmark
    "M"  'bookmark-set
    "b"  'consult-buffer
    "go" 'consult-outline

    ;; s-expression related commands
    "su" 'backward-up-list
    "sd" 'down-list
    "sf" 'forward-sexp
    "st" 'transpose-sexps
    "sb" 'backward-sexp
    "sm" 'mark-sexp
    "sk" 'kill-sexp
    "sy" 'sp-copy-sexp

    ;; Wrapping commands
    "wr" 'sp-wrap-round
    "ws" 'sp-wrap-square
    "wc" 'sp-wrap-curly
    "wx" 'sp-unwrap-sexp
    "ww" 'sp-rewrap-sexp
    "wq" 'wrap-double-quote
    "wb" 'wrap-back-quote

    ;; Org keys
    "oc" 'org-capture
    "oa" 'org-agenda
    "oe" 'org-export

    ;; Project/projectile commands
    "." 'xref-find-definitions
    "," 'xref-pop-marker-stack
    ;; "pp" 'counsel-projectile-switch-project
    ;; "pf" 'counsel-projectile-find-file
    "pp" 'projectile-switch-project
    "pf" 'projectile-find-file
    "pt" 'projectile-toggle-between-implementation-and-test))

(use-package evil
  :bind
  (:map evil-normal-state-map
	("u" . 'undo-fu-only-undo)
	("C-r" . 'undo-fu-only-redo))

  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-digraphs-table-user '(((?. ?.) . ?\x2026)
				   ((?e ?l) . ?\x2113)))

  :config
  (evil-mode)
  (global-origami-mode)
  (add-hook 'evil-insert-state-exit-hook 'maybe-save-buffer) 
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'git-commit-setup-hook 'evil-insert-state)
  (add-hook 'deft-mode-hook 'evil-emacs-state)
  (global-evil-leader-mode)
  (setq evil-auto-indent nil))

(use-package evil-numbers
  :defer t
  :bind (:map evil-normal-state-map
	      ("C-a" . 'evil-numbers/inc-at-pt)
	      ("C-S-a" . 'evil-numbers/dec-at-pt)))

(use-package evil-args
  :defer t
  :bind
  (:map evil-inner-text-objects-map
	("a" . 'evil-inner-arg)
	:map evil-outer-text-objects-map
	("a" . 'evil-outer-arg)
	:map evil-normal-state-map
	("L" . 'evil-forward-arg)
	("H" . 'evil-backward-arg)
	("K" . 'evil-jump-out-args)
	:map evil-motion-state-map
	("L" . 'evil-forward-arg)
	("H" . 'evil-backward-arg)))

(use-package evil-nerd-commenter
  :defer t)

(use-package magit
  :defer t
  :bind (("C-x g" . 'magit-status)
	 ("s-g" . 'magit-status)))

(use-package git-timemachine
  :defer t)

(use-package avy
  :bind (("C-c j" . 'avy-goto-line)
	 ("C-M-j" . 'avy-goto-char)
	 ("C-c J" . 'avy-goto-word-0)
	 ("s-j" . 'avy-goto-line)
	 ("s-J" . 'avy-goto-word-0)
	 ("C-s-j" . 'avy-goto-char)))

(use-package ace-window
  :bind (("M-o" . 'ace-window))
  :config
  (setq aw-background nil)
  (ace-window-display-mode +1))

(use-package which-key
  :diminish ""
  :config
  (which-key-mode))

(use-package deft
  :defer t
  :config
  (setq deft-extensions '("org" "md" "txt" "tex"))
  (setq deft-new-file-format "%Y-%m-%d")
  (setq deft-default-extension "org")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t))

(use-package olivetti
  :diminish "»O«"
  :defer t)

(use-package define-word
  :defer t
  :bind (("M-#" . define-word)
	 ("C-M-3" . define-word-at-point)))

(custom-set-faces
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#121212" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Input Mono"))))
 '(company-tooltip ((t (:background "#3b4252" :foreground "#D8DEE9"))))
 '(elixir-atom-face ((t (:foreground "#88c0d0" :weight normal))))
 '(fixed-pitch ((t nil)))
 '(font-lock-comment-face ((t (:foreground "#636f8a"))))
 '(font-lock-doc-face ((t (:foreground "#88e088"))))
 '(font-lock-function-name-face ((t (:foreground "#88c0d0"))))
 '(font-lock-keyword-face ((t (:foreground "#b48ead"))))
 '(font-lock-type-face ((t (:foreground "#a3be8c"))))
 '(font-lock-warning-face ((t (:foreground "#d08770"))))
 '(fringe ((t (:background "#171717" :foreground "#545454"))))
 '(highlight ((t (:background "#3b4252" :foreground "#88C0D0"))))
 '(hl-line ((t (:extend t :background "#191919"))))
 '(italic ((t (:foreground "#bfefff" :slant italic))))
 '(magit-section-highlight ((t (:extend t :background "#2e3440"))))
 '(magit-tag ((t (:foreground "#fcec2a"))))
 '(markdown-header-face-1 ((t (:inherit org-level-1 :height 1.0))))
 '(markdown-header-face-2 ((t (:inherit org-level-2 :height 1.0))))
 '(markdown-header-face-3 ((t (:inherit org-level-3 :height 1.0))))
 '(markdown-header-face-4 ((t (:inherit org-level-4 :height 1.0))))
 '(markdown-header-face-5 ((t (:inherit org-level-5 :height 1.0))))
 '(markdown-header-face-6 ((t (:inherit org-level-6 :height 1.0))))
 '(mode-line ((t (:background "#4C566A" :foreground "#d8dee9"))))
 '(mode-line-inactive ((t (:background "#2e3440" :foreground "#5e81ac"))))
 '(mu4e-thread-folding-child-face ((t (:extend t :background "#202020" :underline nil))) t)
 '(mu4e-thread-folding-root-unfolded-face ((t (:extend t :background "#404040" :overline nil :underline nil))) t)
 '(org-agenda-date ((t (:extend t :foreground "#88c0d0" :underline t :height 1.1))))
 '(org-agenda-date-today ((t (:extend t :foreground "#569cd6" :inverse-video t :underline nil :weight normal :height 1.1))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "#4a708b" :slant italic :weight normal))))
 '(org-agenda-done ((t (:foreground "#3b4252"))))
 '(org-block ((t (:extend t :background "#0c0c0c" :foreground "#e8e8e8"))))
 '(org-code ((t (:foreground "#b0ffa0"))))
 '(org-column ((t (:background "grey14" :strike-through nil :underline nil :slant normal :weight normal))))
 '(org-headline-done ((t (:foreground "#556655"))))
 '(org-level-1 ((t (:extend nil :foreground "#6cecff" :weight normal :height 1.1))))
 '(org-level-2 ((t (:extend nil :foreground "#8cccfe" :weight normal))))
 '(org-link ((t (:underline "#88c0d0"))))
 '(org-priority ((t (:foreground "#b48ead"))))
 '(org-quote ((t (:inherit org-block :foreground "#aae0aa" :slant italic))))
 '(org-scheduled ((t (:foreground "#a3be8c"))))
 '(org-scheduled-today ((t (:foreground "#ecec9a" :weight normal :height 1))))
 '(org-table ((t (:background "#202020" :foreground "#e8e8e8"))))
 '(org-time-grid ((t (:foreground "#ebcb8b"))))
 '(org-todo ((t (:foreground "#d08770" :weight bold))))
 '(org-upcoming-deadline ((t (:foreground "#d08770"))))
 '(org-upcoming-distant-deadline ((t (:foreground "#c0c0c0"))))
 '(org-verbatim ((t (:foreground "#b0b0b0"))))
 '(org-warning ((t (:foreground "#bf616a" :underline nil))))
 '(proof-locked-face ((t (:extend t :background "#101430"))))
 '(racket-xp-unused-face ((t (:underline (:color "yellow" :style wave)))))
 '(region ((t (:extend t :background "#2e3440"))))
 '(show-paren-match-expression ((t (:background "#282828"))))
 '(sp-pair-overlay-face ((t (:background "#03040a"))))
 '(term-color-black ((t (:background "#404040" :foreground "#404040"))))
 '(underline ((t (:underline t)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(company-box-doc-delay 0.3)
 '(company-box-show-single-candidate 'always)
 '(company-show-numbers t)
 '(counsel-rg-base-command
   "rg -M 200 --with-filename --no-heading --line-number --color never %s")
 '(custom-safe-themes
   '("e3b2bad7b781a968692759ad12cb6552bc39d7057762eefaf168dbe604ce3a4b" default))
 '(default-input-method "TeX")
 '(deft-auto-save-interval 30.0)
 '(dired-use-ls-dired nil)
 '(find-file-visit-truename t)
 '(frame-resize-pixelwise t)
 '(highlight-indent-guides-method 'character)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message
   ";; This space intentionally left blank. Try \\[find-file].

")
 '(ispell-query-replace-choices t)
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$" "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$" "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$" "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.deps$" "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$" "[/\\\\]\\.reference$" "[/\\\\]_build$"))
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-ui-doc-delay 0.5)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-position 'at-point)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta)) ((control) . text-scale)))
 '(mu4e-bookmarks
   '((:name "Inbox" :query "maildir:/INBOX" :key 105)
     (:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key 117)
     (:name "Today's messages" :query "date:today..now" :key 116)
     (:name "Last 7 days" :query "date:7d..now" :key 119)
     (:name "Messages with images" :query "mime:image/*" :hide-unread t :key 112)
     (:name "Drafts" :query "maildir:/Drafts" :key 100)))
 '(mu4e-headers-fields
   '((:human-date . 16)
     (:size . 8)
     (:flags . 6)
     (:maildir . 15)
     (:from . 30)
     (:subject)))
 '(ns-use-native-fullscreen nil)
 '(olivetti-body-width 124)
 '(org-agenda-files
   '("~/Sync/beorg/inbox.org" "~/Sync/beorg/general.org" "~/Sync/Dropbox/beorg/for_later.org" "~/Sync/Dropbox/undergrad_research/research-notes/research_tasks.org" "~/Sync/beorg/school.org" "~/Sync/beorg/family_shared.org" "~/Sync/beorg/projects.org" "~/Sync/beorg/work.org"))
 '(org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t%-6e% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
 '(org-fontify-quote-and-verse-blocks t)
 '(org-startup-folded t)
 '(org-tags-column -90)
 '(scheme-program-name "racket")
 '(sentence-end-double-space nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(visible-bell t))
