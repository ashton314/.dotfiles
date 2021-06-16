(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package lsp-mode
  :config
  (add-to-list 'exec-path "~/Sync/repos/elixir-ls/release"))

(use-package tree-sitter
  :init
  (defun setup-tree-sitter ()
    (tree-sitter-mode)
    (tree-sitter-hl-mode))
  :config
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  :hook ((ruby-mode . setup-tree-sitter)))

(use-package tree-sitter-langs
  :defer t)

(defvar elixir-outline-regexp
  (concat "^[[:space:]]*\\("
	  "def\\(\\|p\\|callback\\|delegate\\|module\\|impl\\|overridable\\|exception\\|struct\\|guard\\|guardp\\|record\\|recordp\\|macro\\|macrop\\|macrocallback\\|protocol\\)"
	  "\\|describe\\|test\\|setup"
	  "\\|@type"
	  "\\)\\([[:space:]]\\|(\\)"))

(defvar scribble-outline-regexp
  (concat
   "^[[:space:]]*\\("
   "@(?define\\|"
   "@\\(sub\\)*section{\\|"
   "@\\(title\\|subtitle\\|abstract\\)"
   "\\)"
   ))

(defun configure-elixir-mode ()
  (setq-local outline-regexp elixir-outline-regexp))

(defun configure-scribble-mode ()
  (setq-local outline-regexp scribble-outline-regexp))

(use-package elixir-mode
  :defer t
  :config
  (add-hook 'elixir-mode-hook #'configure-elixir-mode))

(use-package dockerfile-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package json-mode
  :defer t)
