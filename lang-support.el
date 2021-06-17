(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package lsp-mode
  :after (exec-path-from-shell)
  :diminish " lsp"
  :bind (("C-c C-d" . lsp-describe-thing-at-point))
  :config
  (setq lsp-elixir-server "~/Sync/repos/elixir-ls/release/language_server.sh")
  (setq lsp-file-watch-threshold 10000)
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

(defun format-elixir-file ()
  (setq elixir-format-arguments
	(list "--dot-formatter"
	      (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs"))))

(use-package elixir-mode
  :defer t
  :bind (("C-M-q" . elixir-mode-fill-doc-string))
  :hook ((elixir-mode . configure-elixir-mode)
	 (elixir-mode . lsp-deferred)
	 (elixir-format . format-elixir-file))
  :config
  (add-hook 'elixir-mode-hook
	    (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

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
