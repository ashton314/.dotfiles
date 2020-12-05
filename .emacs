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
;; (setq gc-cons-threshold 10000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; straight.el setup <<straight.el>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bootstrap-version)
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

;; Mode-line nicities, basic customizations
(display-time)
(setq column-number-mode t)
(setq line-number-mode t)
(setq tool-bar-mode -1)
(global-auto-revert-mode)
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

;(straight-use-package '(consult :type git :host github :repo "minad/consult"))
;(consult-annotate-mode)
;(setf (alist-get 'execute-extended-command consult-annotate-alist) #'consult-annotate-command-full)
(use-package consult
  :straight '(consult :type git :host github :repo "minad/consult")

  ;; Replace bindings. Lazily loaded due to use-package.
  :bind (("s-o" . consult-outline)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g o" . consult-outline) ;; "M-s o" is a good alternative
         ("M-g m" . consult-mark)    ;; "M-s m" is a good alternative
         ("M-g l" . consult-line)    ;; "M-s l" is a good alternative
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)

  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config

  ;; Optionally enable previews. Note that individual previews can be disabled
  ;; via customization variables.

  ;; Broken 2020-12-05
  ;(consult-preview-mode)
  )

(use-package marginalia
  :straight '(marginalia :type git :host github :repo "minad/marginalia" :branch "main")
  
  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.

  ; Super slow 2020-12-05
  ;(marginalia-mode)

  ;; Enable richer annotations for M-x.
  ;; Only keybindings are shown by default, in order to reduce noise for this very common command.
  ;; * marginalia-annotate-symbol: Annotate with the documentation string
  ;; * marginalia-annotate-command-binding (default): Annotate only with the keybinding
  ;; * marginalia-annotate-command-full: Annotate with the keybinding and the documentation string

  ;; As of 2020-12-05, this makes M-x hang for a good second or two
  ;;(setf (alist-get 'command marginalia-annotator-alist) #'marginalia-annotate-command-full)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GCC Emacs config <<gcc emacs>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is necessary to get LSP working properly
(use-package exec-path-from-shell
  :config
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
(straight-use-package 'diminish)

;; Evil
(straight-use-package 'evil)

;; Org
(straight-use-package 'org)
(straight-use-package 'org-ql)
;; (straight-use-package '(elgantt :type git :host github :repo "legalnonsense/elgantt"))
(straight-use-package 'org-fragtog)
;;(setq org-latex-create-formula-image-program 'dvisvgm) ; doesn't work with the mac version

;; Org-Babel
(straight-use-package 'ob-elixir)

;; Org-Roam
(unless (or (equal org-roam-directory "")
	    (not enable-org-roam-on-startup))
  (straight-use-package 'org-roam))

;; Selectrum
(straight-use-package 'selectrum)
(selectrum-mode +1)
(straight-use-package 'selectrum-prescient)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

;; Company
(straight-use-package 'company)
;; (straight-use-package 'company-box)
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

(straight-use-package 'company-prescient)
(company-prescient-mode +1)

;; Programming language packages
(straight-use-package 'elixir-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'web-mode)
(straight-use-package 'json-mode)
(straight-use-package 'proof-general)	; Coq IDE-ness
(straight-use-package 'company-coq)

;; lsp-mode
(straight-use-package 'lsp-mode)
(add-to-list 'exec-path "~/Sync/repos/elixir-ls/release")
(straight-use-package 'lsp-ui)

;; Do not use this one!
;; (straight-use-package 'lsp-elixir)

;; Deft
(straight-use-package 'deft)
(setq deft-extensions '("org" "md" "txt" "tex"))
(setq deft-new-file-format "%Y-%m-%d")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)

;; Ace, Avy
(straight-use-package 'ace-window)
(setq aw-background nil)
(ace-window-display-mode +1)

;; Multiple-cursors
(straight-use-package 'multiple-cursors)
;; TODO: keybindings

;; Projectile
(straight-use-package 'projectile)
(setq projectile-completion-system 'ivy)
(define-key global-map (kbd "C-x p") 'projectile-command-map)
(projectile-mode +1)
(diminish 'projectile-mode " proj")

;; Yasnippets
(straight-use-package 'yasnippet)
(yas-global-mode +1)
(straight-use-package 'auto-yasnippet)

;; Searching/mass editing
(straight-use-package 'counsel)
(straight-use-package 'swiper)
(define-key global-map (kbd "C-s") 'swiper)
(straight-use-package 'counsel-projectile)
(straight-use-package 'wgrep)

;; Magit (Mah-jit---like "magi{-c+t}")
(straight-use-package 'magit)
(define-keys-globally 'magit-status "C-x g" "s-g")

(straight-use-package 'git-timemachine)

;; Programming
(straight-use-package 'smartparens)
(smartparens-global-mode +1)
(straight-use-package 'racket-mode)
(straight-use-package 'vterm)

;; Writing
(straight-use-package 'olivetti)
(straight-use-package 'define-word)
(straight-use-package 'lorem-ipsum)
(straight-use-package 'pandoc)

;; Dependencies for certain functions I've written
(straight-use-package 'f)

;; Polymode
(straight-use-package 'polymode)
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
;; Custom key definitions <<key bindings>>
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
(define-key global-map (kbd "s-j") 'avy-goto-line)

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
(define-key global-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)

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

;; Links
(define-key global-map (kbd "C-c o l") 'org-store-link)
(define-key global-map (kbd "C-c L") 'org-insert-link-global)
(define-key global-map (kbd "C-c O") 'org-open-at-point-global)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode customizations <<org mode>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        ;("p" "Programming Language Research")
	("e" "ENGL 211 (Rhetoric and Civ)" entry (file+headline org-school-file "ENGL 211: Rhretoric and Civilization 1")
	 "** TODO %? :homework:engl_211:\n %U\n %i\n %a")
	("p" "PHSCS 121 (Intro to Physics)" entry (file+headline org-school-file "PHSCS 121: Introduction to Newtonian Mechanics")
	 "** TODO %? :homework:phscs_121:\n %U\n %i\n %a")
	("o" "Other")
	("g" "General Homework" entry (file+headline org-school-file "General")
	 "** TODO %?\n   %U\n%i\n%a")
        ;; CS
        ("cf" "CS 401R (Foundations)" entry (file+headline org-school-file "CS 401R: Software Foundations")
         "** TODO %? :homework:cs_401r:\n %U\n %i\n %a")
        ("cd" "CS 404 (Ethics)" entry (file+headline org-school-file "CS 404: Ethics & Computers in Society")
         "** TODO %? :homework:cs_404:\n %U\n %i\n %a")
	;; Other
	("os" "STDEV 318 (Grad School Prep)" entry (file+headline org-school-file "STDEV 318: Graduate School Preparation")
	 "** TODO %? :homework:stdev_318:\n %U\n %i\n %a")
	("og" "SWELL 132 (Golf)" entry (file+headline org-school-file "SWELL 132: Intermediate Golf")
	 "** TODO %? :homework:swell_132:\n %U\n %i\n %a")
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
;; (setq org-refile-targets
;;       `(((,org-project-notes-file) . (:level . 0))
;;         ((,org-notes-file) . (:level . 0))
;;         ((,org-general-notes-file) . (:level . 1))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook defintions <<hooks>>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org-roam
(defun org-roam-setup-hooks ()
  "Install org-roam hooks"
  (interactive)
  (straight-use-package 'org-roam)
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
 '(lsp-file-watch-ignored
   '("[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$" "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$" "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$" "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.deps$" "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$" "[/\\\\]\\.reference$" "[/\\\\]_build$"))
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta)) ((control) . text-scale)))
 '(ns-use-native-fullscreen nil)
 '(olivetti-body-width 80)
 '(org-agenda-files
   '("~/Sync/beorg/mobile_inbox.org" "~/Sync/beorg/general.org" "~/Sync/Dropbox/beorg/for_later.org" "~/Sync/Dropbox/undergrad_research/research-notes/research_tasks.org" "~/Sync/beorg/school.org" "~/Sync/beorg/family_shared.org" "~/Sync/beorg/projects.org" "~/Sync/beorg/work.org"))
 '(org-fontify-quote-and-verse-blocks t)
 '(org-startup-folded t)
 '(org-tags-column -100)
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
 '(italic ((t (:foreground "#ffc125" :slant italic))))
 '(magit-tag ((t (:foreground "#fcec2a"))))
 '(markdown-header-face-1 ((t (:inherit org-level-1 :height 1.0))))
 '(markdown-header-face-2 ((t (:inherit org-level-2 :height 1.0))))
 '(markdown-header-face-3 ((t (:inherit org-level-3 :height 1.0))))
 '(markdown-header-face-4 ((t (:inherit org-level-4 :height 1.0))))
 '(markdown-header-face-5 ((t (:inherit org-level-5 :height 1.0))))
 '(markdown-header-face-6 ((t (:inherit org-level-6 :height 1.0))))
 '(org-block ((t (:extend t :background "#0c0c0c" :foreground "#e8e8e8"))))
 '(org-code ((t (:foreground "#b0ffa0"))))
 '(org-headline-done ((t (:foreground "#556655"))))
 '(org-level-1 ((t (:extend nil :foreground "#6cecff" :weight normal :height 1.1))))
 '(org-level-2 ((t (:extend nil :foreground "#8cccfe" :weight normal))))
 '(org-priority ((t (:foreground "#ee7600"))))
 '(org-quote ((t (:inherit org-block :foreground "#aae0aa" :slant italic))))
 '(org-scheduled-today ((t (:foreground "#4cff5a" :weight normal :height 1.2))))
 '(org-table ((t (:background "#202020" :foreground "#e8e8e8"))))
 '(org-verbatim ((t (:foreground "#b0b0b0"))))
 '(proof-locked-face ((t (:extend t :background "#101430"))))
 '(selectrum-primary-highlight ((t (:foreground "#98f5ff" :underline t))))
 '(selectrum-secondary-highlight ((t (:inherit selectrum-primary-highlight :weight bold))))
 '(show-paren-match-expression ((t (:background "#282828"))))
 '(sp-pair-overlay-face ((t (:background "#254545"))))
 '(term-color-black ((t (:background "#404040" :foreground "#404040"))))
 '(underline ((t (:underline "#ffc125")))))
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
