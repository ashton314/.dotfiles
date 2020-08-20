;; Emacs Config file
;; Ashton Wiersdorf

;; Set GC level higher to prevent so many garbage collection cycles
;; during startup and elsewhere.
(setq gc-cons-threshold 10000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; straight.el setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Theme
(straight-use-package 'vscode-dark-plus-theme)
(load-theme 'vscode-dark-plus t)

;; Mode-line nicities
(display-time)
(setq column-number-mode t)
(setq line-number-mode t)
(setq tool-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special-Use Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GUI-only customizations
(when (and (display-graphic-p) (file-exists-p "~/.dotfiles/.emacs_gui"))
  (load-file "~/.dotfiles/.emacs_gui"))

;; Device-specific variables
(when (file-exists-p "~/.emacs.d/device_vars.el")
  (load-file "~/.emacs.d/device_vars.el"))

;; Aux functions
(when (file-exists-p "~/.dotfiles/emacs_aux.el")
  (load-file "~/.dotfiles/emacs_aux.el"))

(when (file-exists-p "~/.dotfiles/functions.el")
  (load-file "~/.dotfiles/functions.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GCC Emacs config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
      (message "Native comp is available")
      (add-to-list 'exec-path (expand-file-name "~/homebrew/opt/gccemacs/bin"))
      (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
                                     (when (getenv "LIBRARY_PATH")
                                       ":")
                                     (car (file-expand-wildcards
                                           (expand-file-name "~/homebrew/opt/gcc/lib/gcc/*")))))
      ;; Only set after LIBRARY_PATH can find gcc libraries.
      (setq comp-deferred-compilation t))
  (message "Native comp is *not* available"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Diminish
(straight-use-package 'diminish)

;; Org
(straight-use-package 'org)

;; Selectrum
(straight-use-package 'selectrum)
(selectrum-mode +1)
(straight-use-package 'selectrum-prescient)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

;; Company
(straight-use-package 'company)
(straight-use-package 'company-box)
(global-company-mode +1)
(diminish 'company-mode " c")
(add-hook 'company-mode-hook '(lambda () (company-box-mode) (diminish 'company-box-mode " cbox")))

(straight-use-package 'company-prescient)
(company-prescient-mode +1)

;; Programming language packages
(straight-use-package 'elixir-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'web-mode)
(straight-use-package 'json-mode)

;; lsp-mode
(straight-use-package 'lsp-mode)
(add-to-list 'exec-path "~/Sync/repos/elixir-ls/release")

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

(straight-use-package 'yasnippet)
(yas-global-mode +1)

;; Searching
(straight-use-package 'counsel)
(straight-use-package 'swiper)
(define-key global-map (kbd "C-s") 'swiper)
(straight-use-package 'counsel-projectile)

;; magit
(straight-use-package 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)

;; Programming
(straight-use-package 'smartparens)
(smartparens-global-mode +1)
(straight-use-package 'racket-mode)
(straight-use-package 'vterm)

;; Writing
(straight-use-package 'olivetti)
(straight-use-package 'define-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Key Definitions (key bindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook defintions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapc (lambda (mode)
       (add-hook mode
                 '(lambda () (progn
                               (sp-pair "`" nil :actions :rem)
                               (sp-pair "'" nil :actions :rem)))))
     '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook racket-mode-hook))

(add-hook 'company-mode-hook
          '(lambda ()
             (progn
               (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
               (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta)) ((control) . text-scale)))
 '(ns-use-native-fullscreen nil)
 '(frame-resize-pixelwise t)
 '(counsel-rg-base-command
   "rg -M 200 --with-filename --no-heading --line-number --color never %s")
 '(ispell-query-replace-choices t)
 '(company-box-doc-delay 0.3)
 '(company-box-show-single-candidate 'always)
 '(company-idle-delay 0)
 '(company-show-numbers t)
 '(counsel-projectile-mode t nil (counsel-projectile))
 '(deft-auto-save-interval 30.0)
 '(dired-use-ls-dired nil)
 '(find-file-visit-truename t)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message
   ";; This space intentionally left blank. Try \\[find-file].

")
 '(olivetti-body-width 80)
 '(org-agenda-files
   '("~/Sync/beorg/mobile_inbox.org" "~/Sync/beorg/general.org" "~/Sync/Dropbox/beorg/for_later.org" "~/Sync/Dropbox/undergrad_research/research-notes/research_tasks.org" "~/Sync/beorg/school.org" "~/Sync/beorg/family_shared.org" "~/Sync/beorg/projects.org" "~/Sync/beorg/work.org"))
 '(org-fontify-quote-and-verse-blocks t)
 '(org-tags-column -100)
 '(scheme-program-name "racket")
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#171717" :foreground "#fafafa"))))
 '(italic ((t (:foreground "#ffc125" :slant italic))))
 '(show-paren-match-expression ((t (:background "#282828"))))
 '(sp-pair-overlay-face ((t (:background "#254545"))))
 '(term-color-black ((t (:background "#404040" :foreground "#404040"))))
 '(underline ((t (:underline "#ffc125"))))
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#121212" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Input Mono")))))
