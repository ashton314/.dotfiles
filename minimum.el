(setq gc-cons-threshold 500000000)

(display-time)
(setq column-number-mode t)
(setq line-number-mode t)
(setq tool-bar-mode -1)
(global-auto-revert-mode)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(defvar evil-auto-save-buffer-modes '(text-mode scribble-mode org-mode markdown-mode racket-mode emacs-lisp-mode))

;; Setup package manager
(defvar bootstrap-version)
(setq straight-repository-branch "develop")

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Theme
(use-package nord-theme
  :config
  (load-theme 'nord t))

;; Code

(use-package projectile
  :defer t
  :diminish " p"
  :bind (("C-x p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'selectrum))

(use-package racket-mode
  :defer t)

(use-package smartparens
  :diminish ""
  :config
  (smartparens-global-mode +1))

;; Completion

(use-package vertico
  :defer t
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package savehist
  :defer t
  :init
  (savehist-mode))

(use-package corfu
  :config
  (corfu-global-mode))

(use-package dabbrev
  :defer t
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package emacs
  :defer t
  :init
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

(use-package diminish)
(diminish 'eldoc-mode "")

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
  (setq evil-digraphs-table-user '(((?. ?.) . ?\x2026)))

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

(setq gc-cons-threshold 800000)

(custom-set-faces
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#121212" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Input Mono"))))
 '(fringe ((t (:background "#171717" :foreground "#545454"))))
 '(region ((t (:extend t :background "#2e3440"))))
 '(mode-line ((t (:background "#4C566A" :foreground "#d8dee9"))))
 '(mode-line-inactive ((t (:background "#2e3440" :foreground "#5e81ac"))))
 )
