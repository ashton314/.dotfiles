(setq gc-cons-threshold 500000000)	; ~half a gig

(load-file "~/.dotfiles/core.el")
(load-file "~/.dotfiles/keybindings.el")
(load-file "~/.dotfiles/lang-support.el")
;; (load-file "~/.dotfiles/email-setup.el")
;; (load-file "~/.dotfiles/org-customizations.el")
(load-file "~/.emacs.d/device_vars.el")

(setq gc-cons-threshold 800000)
