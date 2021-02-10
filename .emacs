;; Emacs Config File
;; Ashton Wiersdorf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table of Contents
;;
;;  - Straight setup [[straight.el]]
;;  - Default device-specific variables [[default vars]]
;;  - Special-use files [[special files]]
;;  - GCC Emacs config [[gcc emacs]]
;;  - Packages [[packages]]
;;  - Custom key definitions [[key bindings]]
;;  - Org-mode customizations [[org mode]]
;;  - Hook defintions [[hooks]]
;;  - Customizations [[custom]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Set GC level higher to prevent so many garbage collection cycles
;; during startup and elsewhere.
(setq gc-cons-threshold 134217728)	; Got value from DOOM-Emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; straight.el setup <<straight.el>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove old org version from load path
(when-let (orglib (locate-library "org" nil load-path))
  (setq-default load-path (delete (substring (file-name-directory orglib) 0 -1)
                                  load-path)))

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
(straight-use-package 'vscode-dark-plus-theme)
(load-theme 'vscode-dark-plus t)

;; Load org here so we can use this version elsewhere
(use-package org
  ;; Do not defer so we get the right version
  :config
  ;; enable exporting of colors!
  (require 'ox-latex)
  ;(add-to-list 'org-latex-packages-alist '("" "minted"))
  ;(setq org-latex-listings 'minted) 

  (setq org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-src-fontify-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (latex . t))))

;; Mode-line nicities, basic customizations
(display-time)
(setq column-number-mode t)
(setq line-number-mode t)
(setq tool-bar-mode -1)
(global-auto-revert-mode)

;; Put the underline at the descent line, rather than the baseline
(setq x-underline-at-descent-line t)

;; This stays off: I turn this on so that Emacs acts a little bit more
;; like a "conventional" word processor with nicely wrapped lines. I
;; enable it in markdown and org modes. Most of the time when I'm
;; programming, I want it to stay off, however.
;(global-visual-line-mode +1)

;; Error squelching
(setq fill-prefix "") ; See https://emacs.stackexchange.com/questions/38941/wrong-type-argument-char-or-string-p-nil-when-doing-ret-in-org-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default device-specific variables <<default vars>>
;;
;; These are variables that point to paths on my filesystem. These are
;; those that need to be changed on a per-install basis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq deft-directory "")
(setq lsp-java-server-install-dir "")
(setq lsp-elixir-server "")
(setq lsp-file-watch-threshold "")
(setq org-directory "")
(setq scripture-directory "")
(setq org-roam-directory "")
(setq enable-org-roam-on-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special-use files <<special files>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-when-there (filename)
  (when (file-exists-p filename)
    (load-file filename)))

;; GUI-only customizations
(when (and (display-graphic-p) (file-exists-p "~/.dotfiles/.emacs_gui"))
  (load-file "~/.dotfiles/.emacs_gui"))

;; Device-specific variables
(load-when-there "~/.emacs.d/device_vars.el")

;; Aux functions
(load-when-there "~/.dotfiles/emacs_aux.el")

(load-when-there "~/.dotfiles/functions.el")

;; Gilded Selection (my package)
;(straight-use-package '(gilded-select :type git :host github :repo "ashton314/gilded-select"))
;(gilded-select-mode +1)

(use-package consult
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
         ("M-g l" . consult-line)          ;; "M-s l" is a good alternative.
         ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
         ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
         ("M-g r" . consult-git-grep)      ;; or consult-grep, consult-ripgrep
	 ("s-r r" . consult-ripgrep)
         ("M-g f" . consult-find)          ;; or consult-locate, my-fdfind
         ("M-g i" . consult-project-imenu) ;; or consult-imenu
         ("M-g e" . consult-error)
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Custom command wrappers. It is generally encouraged to write your own
  ;; commands based on the Consult commands. Some commands have arguments which
  ;; allow tweaking. Furthermore global configuration variables can be set
  ;; locally in a let-binding.
  (defun my-fdfind (&optional dir)
    (interactive "P")
    (let ((consult-find-command '("fdfind" "--color=never" "--full-path")))
      (consult-find dir)))

  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)

  ;; Configure register preview function.
  ;; This gives a consistent display for both `consult-register' and
  ;; the register preview when editing registers.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview)

  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config

  ;; Configure preview. Note that the preview-key can also be configured on a
  ;; per-command basis via `consult-config'.
  ;; The default value is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-p"))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

  ;; Optionally configure narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optional configure a view library to be used by `consult-buffer'.
  ;; The view library must provide two functions, one to open the view by name,
  ;; and one function which must return a list of views as strings.
  ;; Example: https://github.com/minad/bookmark-view/
  ;; (setq consult-view-open-function #'bookmark-jump
  ;;       consult-view-list-function #'bookmark-view-names)

  ;; Optionally configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package embark
  :ensure t
  :bind
  ("C-S-a" . embark-act)               ; pick some comfortable binding
  :config
  ;; For Selectrum users:
  (defun current-candidate+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidate))))

  (add-hook 'embark-target-finders #'current-candidate+category)

  (defun current-candidates+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidates
             ;; Pass relative file names for dired.
             minibuffer-completing-file-name))))

  (add-hook 'embark-candidate-collectors #'current-candidates+category)

  ;; No unnecessary computation delay after injection.
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GCC Emacs config <<gcc emacs>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is necessary to get LSP working properly
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
      (message "Native comp is available")
      ;; Only set after LIBRARY_PATH can find gcc libraries.
      (setq comp-speed 2)
      (setq comp-deferred-compilation t))
  (message "Native comp is *not* available"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages <<packages>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Diminish
(use-package diminish)

;; Evil
(use-package evil
  :defer t

  :init
  (setq evil-respect-visual-line-mode t)
  ;; (setq evil-undo-system 'undo-fu)

  :config
  ;; (global-undo-tree-mode -1)
  (setq evil-auto-indent nil))

(use-package org-ql
  :defer t)
;; (straight-use-package '(elgantt :type git :host github :repo "legalnonsense/elgantt"))
(use-package org-fragtog
  :defer t)
;;(setq org-latex-create-formula-image-program 'dvisvgm) ; doesn't work with the mac version

;; Org-Babel
(use-package ob-elixir)

;; Org-Roam
(unless (or (equal org-roam-directory "")
	    (not enable-org-roam-on-startup))
  (use-package org-roam))

;; Selectrum
(use-package selectrum)
(selectrum-mode +1)
(use-package selectrum-prescient)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

;; Company
(use-package company)
;; (use-package company-box)
(global-company-mode +1)
(diminish 'company-mode " c")
(add-hook 'company-mode-hook
	  '(lambda ()
             (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
             (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

	     ;; Alternate, evil-like bindings
             (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
             (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
	     ;; (company-box-mode)
	     ;; (diminish 'company-box-mode " cbox")
	     ))

(use-package company-prescient)
(company-prescient-mode +1)

;; Programming language packages
(use-package elixir-mode)
(use-package dockerfile-mode)
(use-package yaml-mode)
(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))
(use-package json-mode)
;(use-package proof-general)	; Coq IDE-ness
;(use-package company-coq)

;; lsp-mode
(use-package lsp-mode
  :config
  (add-to-list 'exec-path "~/Sync/repos/elixir-ls/release"))
(use-package lsp-ui)

;; Do not use this one!
;; (use-package lsp-elixir)

;; LSP goodies for python
(use-package lsp-pyright
  :hook
  (python-mode .
	       (lambda ()
		 (setq indent-tabs-mode nil)
		 (setq tab-width 4)
		 (setq python-indent-offset 4)
                 (require 'lsp-pyright)
                 (lsp-deferred))))

(use-package highlight-indent-guides
  :hook
  (python-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top))

;; Deft
(use-package deft
  :defer t
  :config
  (setq deft-extensions '("org" "md" "txt" "tex"))
  (setq deft-new-file-format "%Y-%m-%d")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t))

;; Ace, Avy
(use-package ace-window)
(setq aw-background nil)
(ace-window-display-mode +1)

;; Multiple-cursors
(use-package multiple-cursors)
;; TODO: keybindings

;; Projectile
(use-package projectile
  :diminish " proj"

  :bind (("C-x p" . projectile-command-map))
  
  :config
  (setq projectile-completion-system 'selectrum))

;; Yasnippets
(use-package yasnippet)
(yas-global-mode +1)
(use-package auto-yasnippet)

;; Searching/mass editing
(use-package counsel)
(use-package swiper)
(define-key global-map (kbd "C-s") 'swiper)
(use-package counsel-projectile)
(use-package wgrep)

;; Magit (Mah-jit---like "magi{-c+t}")
(use-package magit)
(define-keys-globally 'magit-status "C-x g" "s-g")

(use-package git-timemachine)

;; Programming
(use-package smartparens)
(smartparens-global-mode +1)
(use-package racket-mode)
(use-package vterm)

;; Writing
(use-package olivetti)
(use-package define-word)
(use-package lorem-ipsum)
(use-package pandoc-mode)

;; Dependencies for certain functions I've written
(use-package f)

;; Polymode
(use-package polymode)
(define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
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
(define-polymode poly-elixir-mode
  :hostmode 'poly-elixir-hostmode
  :innermodes '(poly-elixir-doc-innermode poly-elixir-template-innermode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom key definitions keybindings <<key bindings>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Searching
(define-key global-map (kbd "C-c C-r") 'counsel-rg)
(define-key global-map (kbd "C-c r") 'counsel-projectile-rg-subdir)

;; Ace window
(define-key global-map (kbd "M-o") 'ace-window)

;; Avy
(define-key global-map (kbd "C-c j") 'avy-goto-line)
(define-key global-map (kbd "C-M-j") 'avy-goto-char)
(define-key global-map (kbd "C-c J") 'avy-goto-word-0)
(define-key global-map (kbd "s-j") 'avy-goto-char)
(define-key global-map (kbd "s-J") 'avy-goto-line)

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
(define-keys-globally 'compile "C-c k")
(define-keys-globally 'wrap-sexp "C-c (")
(define-keys-globally 'sp-unwrap-sexp "C-c <")
(define-keys-globally 'sp-rewrap-sexp "C-c )")
(define-keys-globally 'lsp-describe-thing-at-point "C-c C-d")

(define-keys-globally 'dired "C-x D")
(define-keys-globally 'undo "C-x U")

;; windowing
(define-keys-globally 'previous-multiframe-window "C-x O")
(define-keys-globally 'enlarge-window "C-^")
(define-keys-globally 'sticky-enlarge-window-horizontally "C-x }")
(define-keys-globally 'sticky-shrink-window-horizontally "C-x {")
(define-keys-globally 'shrink-window-horizontally "<f7>")
(define-keys-globally 'balance-windows "<f8>")
(define-keys-globally 'enlarge-window-horizontally "<f9>")

;; auto-yasnippets
(define-keys-globally 'aya-create "C-c y w")
(define-keys-globally 'aya-expand "C-c y y")
(define-keys-globally 'aya-open-line "C-c y o")

;; helpers
(define-keys-globally 'define-word "M-#")
(define-keys-globally 'define-word-at-point "C-M-3")
(define-keys-globally 'query-kill-string "C-x Q")
(define-keys-globally 'insert-file-name "C-c f n")
(define-keys-globally 'string-insert-rectangle "C-c s i r")
(define-keys-globally 'insert-scripture-ref "C-c s r")

;; Registers
(define-keys-globally 'point-to-register "C-c C-SPC")
(define-keys-globally 'jump-to-register "C-c C-j")

;; Links
(define-keys-globally 'org-store-link "C-c o l")
(define-keys-globally 'org-insert-link-global "C-c L")
(define-keys-globally 'org-open-at-point-global "C-c O")

;; Mail
(define-keys-globally 'mu4e "s-m")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode customizations <<org mode>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-export-with-smart-quotes t)

;; Fix strange bug encountered 2021-01-07
(setq org-priority-highest org-highest-priority)
(setq org-priority-lowest org-lowest-priority)

;; Make return key smarter when dealing with lists
(defun org-return--around (old-fn &rest args)
  (let ((context (org-element-lineage (org-element-at-point) '(item))))
    (if (and context (not args))
        (org-insert-item (org-element-property :checkbox context))
      (apply old-fn args))))

(advice-add 'org-return :around 'org-return--around)

;; Exporting with color!

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
                      ("project" . ?j)
                      ("tiny" . ?t)
                      (:endgroup)
                      ;; misc
                      ("meta")
                      ("review")
		      ("german" . ?g)
		      ("reading")
                      ))

(setq org-todo-keywords
      '((sequence "TODO(t)" "BLOCKED(b@)" "IN_PROGRESS(p!)" "|" "DONE(d!)" "WONT_FIX(w@)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I'm going to keep this here because it's a good example of what's									     ;;
;; possible with capture templates.													     ;;
;; 																	     ;;
;; (setq org-capture-templates														     ;;
;;       '(("t" "General Todo" entry (file org-default-notes-file)									     ;;
;; 	 "* TODO %?\n   %U\n%i\n%a")													     ;;
;;         ("s" "Scripture Study" entry (file+headline org-scripture-study-file "HEAD")							     ;;
;;          "** %?\n    %U")														     ;;
;;         ("c" "Computer Science")													     ;;
;;         ;("p" "Programming Language Research")											     ;;
;; 	("e" "ENGL 211 (Rhetoric and Civ)" entry (file+headline org-school-file "ENGL 211: Rhretoric and Civilization 1")		     ;;
;; 	 "** TODO %? :homework:engl_211:\n %U\n %i\n %a")										     ;;
;; 	("p" "PHSCS 121 (Intro to Physics)" entry (file+headline org-school-file "PHSCS 121: Introduction to Newtonian Mechanics")	     ;;
;; 	 "** TODO %? :homework:phscs_121:\n %U\n %i\n %a")										     ;;
;; 	("o" "Other")															     ;;
;; 	("g" "General Homework" entry (file+headline org-school-file "General")								     ;;
;; 	 "** TODO %?\n   %U\n%i\n%a")													     ;;
;;         ;; CS															     ;;
;;         ("cf" "CS 401R (Foundations)" entry (file+headline org-school-file "CS 401R: Software Foundations")				     ;;
;;          "** TODO %? :homework:cs_401r:\n %U\n %i\n %a")										     ;;
;;         ("cd" "CS 404 (Ethics)" entry (file+headline org-school-file "CS 404: Ethics & Computers in Society")			     ;;
;;          "** TODO %? :homework:cs_404:\n %U\n %i\n %a")										     ;;
;; 	;; Other															     ;;
;; 	("os" "STDEV 318 (Grad School Prep)" entry (file+headline org-school-file "STDEV 318: Graduate School Preparation")		     ;;
;; 	 "** TODO %? :homework:stdev_318:\n %U\n %i\n %a")										     ;;
;; 	("og" "SWELL 132 (Golf)" entry (file+headline org-school-file "SWELL 132: Intermediate Golf")					     ;;
;; 	 "** TODO %? :homework:swell_132:\n %U\n %i\n %a")										     ;;
;;         ;; PL Research														     ;;
;;         ("pt" "Research Task" entry (file org-research-tasks)									     ;;
;;          "* TODO %?\n  %U\n %i\n %a")												     ;;
;;         ;; ("pn" "Research Note" entry ;; FIXME: add file										     ;;
;;         ;;  "** %?\n   %U\n %i\n %a")												     ;;
;; 	))																     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	((,org-school-file) . (:level . 1))))
;; (setq org-refile-targets
;;       `(((,org-project-notes-file) . (:level . 0))
;;         ((,org-notes-file) . (:level . 0))
;;         ((,org-general-tasks-file) . (:level . 1))
;;         ((,org-for-later-file) . (:level . 0))
;;         ((,org-family-notes-file) . (:maxlevel . 1))
;;         ((,org-school-file) . (:maxlevel . 1))))

;; org-roam keybindings
(define-key global-map (kbd "C-c o r c") 'org-roam-capture)
(define-key global-map (kbd "C-c o r f") 'org-roam-find-file)
(define-key global-map (kbd "C-c o r i") 'org-roam-insert)

;; org-roam capture templates
(setq org-roam-capture-templates
      '(("d" "default" plain (function org-roam--capture-get-point)
	 "%?"
	 :file-name "%<%Y%m%d%H%M%S>-${slug}"
	 :head "#+title: ${title}\n"
	 :unnarrowed nil)))

;; org agenda enhancements
;; (setq elegant-agenda-font "Input Mono")
;; (setq elegant-agenda-is-mono-font t)
;; (use-package elegant-agenda-mode
;;   :straight (elegant-agenda-mode :type git :host github :repo "justinbarclay/elegant-agenda-mode")
;;   :hook org-agenda-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook defintions <<hooks>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org-roam
(defun org-roam-setup-hooks ()
  "Install org-roam hooks"
  (interactive)
  (use-package org-roam)
  (add-hook 'after-init-hook 'org-roam-mode)
  (add-hook 'org-roam-mode-hook '(lambda () (diminish 'org-roam-mode " roam"))))

(unless (or (equal org-roam-directory "")
	    (not enable-org-roam-on-startup))
  (org-roam-setup-hooks))

;; org roam deft function
(defun roam-deft ()
  "Open up `deft' in the org-roam directory."
  (interactive)
  (let ((deft-directory org-roam-directory))
    (deft)))

(defun chdir-roam ()
  "Open up the org-roam directory. (Probably a hack.)"
  (interactive)
  (find-file org-roam-directory))

;; Coq
(add-hook 'coq-mode-hook
	  '(lambda () (company-coq-mode)))

;; Quotes for lisp-like languages
(mapc (lambda (mode)
       (add-hook mode
                 '(lambda () (progn
                               (sp-pair "`" nil :actions :rem)
                               (sp-pair "'" nil :actions :rem)))))
     '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook racket-mode-hook))

;; Writing modes
(mapc (lambda (mode)
       (add-hook mode
                 '(lambda ()
		    (toggle-truncate-lines -1)
		    (turn-on-visual-line-mode)
		    ;; I don't think this is necessary with visual-line-mode turned on
		    ;(toggle-word-wrap t)
		    )))
     '(markdown-mode-hook org-mode-hook text-mode-hook))

(add-hook 'markdown-mode-hook
	  '(lambda ()
	     (require 'org)
	     (orgtbl-mode)

	     (define-key markdown-mode-map (kbd "C-c f m") 'open-current-file-macdown)
	     (define-key markdown-mode-map [(shift right)] 'org-cycle-list-bullet)
	     (define-key markdown-mode-map [(shift left)] 'org-cycle-list-backwards)))

;; org-mode stuffs
(add-hook 'org-mode-hook
	  '(lambda ()
	     (org-fragtog-mode)
	     (electric-indent-local-mode -1)))

;; lsp
(add-hook 'elixir-mode-hook 'lsp-deferred)

;; These two hooks run `elixir-format` on save. Original post: https://github.com/elixir-editors/emacs-elixir#add-elixir-mode-hook-to-run-elixir-format-on-file-save
(add-hook 'elixir-mode-hook
          (lambda ()
	    (define-key elixir-mode-map (kbd "C-M-q") 'elixir-mode-fill-doc-string)

            (add-hook 'before-save-hook 'elixir-format nil t)))
(add-hook 'elixir-format-hook (lambda ()
                                (setq elixir-format-arguments
                                      (list "--dot-formatter"
                                            (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations <<custom>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fix default directory on macOS with Emacs 27.1
(setq default-directory "~/")

;; Minor speed bump for long lines
(setq bidi-inhibit-bpa t)
;setting bidi-paragraph-direction to 'left-to-right has the same effect
;(setq-default bidi-display-reordering nil)

;; Special mode setup
(add-to-list 'auto-mode-alist '("mutt-" . markdown-mode)) ; email
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.l?eex\\'" . web-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(company-box-doc-delay 0.3)
 '(company-box-show-single-candidate 'always)
 '(company-idle-delay 0.3)
 '(company-show-numbers t)
 '(counsel-projectile-mode t nil (counsel-projectile))
 '(counsel-rg-base-command
   "rg -M 200 --with-filename --no-heading --line-number --color never %s")
 '(default-input-method "TeX")
 '(deft-auto-save-interval 30.0)
 '(dired-use-ls-dired nil)
 '(enable-recursive-minibuffers t)
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
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta)) ((control) . text-scale)))
 '(mu4e-bookmarks
   '((:name "Inbox" :query "maildir:/INBOX" :key 105)
     (:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key 117)
     (:name "Today's messages" :query "date:today..now" :key 116)
     (:name "Last 7 days" :query "date:7d..now" :key 119)
     (:name "Messages with images" :query "mime:image/*" :hide-unread t :key 112)
     (:name "Drafts" :query "maildir:/INBOX.Drafts" :key 100)))
 '(mu4e-headers-fields
   '((:human-date . 16)
     (:size . 8)
     (:flags . 6)
     (:maildir . 20)
;; (:mailing-list . 10)
     (:from . 22)
     (:subject)))
 '(ns-use-native-fullscreen nil)
 '(olivetti-body-width 80)
 '(org-agenda-files
   '("~/Sync/beorg/mobile_inbox.org" "~/Sync/beorg/general.org" "~/Sync/Dropbox/beorg/for_later.org" "~/Sync/Dropbox/undergrad_research/research-notes/research_tasks.org" "~/Sync/beorg/school.org" "~/Sync/beorg/family_shared.org" "~/Sync/beorg/projects.org" "~/Sync/beorg/work.org"))
 '(org-fontify-quote-and-verse-blocks t)
 '(org-startup-folded t)
 '(org-tags-column -90)
 '(scheme-program-name "racket")
 '(sentence-end-double-space nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#121212" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Input Mono"))))
 '(fixed-pitch ((t nil)))
 '(font-lock-doc-face ((t (:foreground "#88e088"))))
 '(fringe ((t (:background "#171717" :foreground "#545454"))))
 '(highlight ((t (:background "#3131b0" :foreground "#f4f4f4" :underline nil))))
 '(hl-line ((t (:extend t :background "#191919"))))
 '(italic ((t (:foreground "#bfefff" :slant italic))))
 '(magit-tag ((t (:foreground "#fcec2a"))))
 '(markdown-header-face-1 ((t (:inherit org-level-1 :height 1.0))))
 '(markdown-header-face-2 ((t (:inherit org-level-2 :height 1.0))))
 '(markdown-header-face-3 ((t (:inherit org-level-3 :height 1.0))))
 '(markdown-header-face-4 ((t (:inherit org-level-4 :height 1.0))))
 '(markdown-header-face-5 ((t (:inherit org-level-5 :height 1.0))))
 '(markdown-header-face-6 ((t (:inherit org-level-6 :height 1.0))))
 '(org-agenda-date ((t (:extend t :foreground "#9cdcfe" :underline t :height 1.1))))
 '(org-agenda-date-today ((t (:extend t :foreground "#569cd6" :inverse-video t :underline nil :weight normal :height 1.1))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "#4a708b" :slant italic :weight normal))))
 '(org-block ((t (:extend t :background "#0c0c0c" :foreground "#e8e8e8"))))
 '(org-code ((t (:foreground "#b0ffa0"))))
 '(org-headline-done ((t (:foreground "#556655"))))
 '(org-level-1 ((t (:extend nil :foreground "#6cecff" :weight normal :height 1.1))))
 '(org-level-2 ((t (:extend nil :foreground "#8cccfe" :weight normal))))
 '(org-priority ((t (:foreground "#ee7600"))))
 '(org-quote ((t (:inherit org-block :foreground "#aae0aa" :slant italic))))
 '(org-scheduled-today ((t (:foreground "#ecec9a" :weight normal :height 1))))
 '(org-table ((t (:background "#202020" :foreground "#e8e8e8"))))
 '(org-upcoming-distant-deadline ((t (:foreground "#c0c0c0"))))
 '(org-verbatim ((t (:foreground "#b0b0b0"))))
 '(org-warning ((t (:foreground "#f16969" :underline nil))))
 '(proof-locked-face ((t (:extend t :background "#101430"))))
 '(selectrum-primary-highlight ((t (:foreground "#98f5ff" :underline t))))
 '(selectrum-secondary-highlight ((t (:inherit selectrum-primary-highlight :weight bold))))
 '(show-paren-match-expression ((t (:background "#282828"))))
 '(sp-pair-overlay-face ((t (:background "#254545"))))
 '(term-color-black ((t (:background "#404040" :foreground "#404040"))))
 '(underline ((t (:underline t)))))
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
