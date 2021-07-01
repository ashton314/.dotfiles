(setq gc-cons-threshold 500000000)	; ~half a gig

(load-file "~/.dotfiles/bootstrap.el")
(load-file "~/.dotfiles/core.el")
(load-file "~/.dotfiles/keybindings.el")
(load-file "~/.dotfiles/lang-support.el")
;; (load-file "~/.dotfiles/org-customizations.el")
(load-file "~/.emacs.d/device_vars.el")

(defvar lazy-email-loadedp nil)
(defun lazy-boot-email ()
  (interactive)
  (when (not lazy-email-loadedp)
    (message "Loading email config...")
    (load-file "~/.dotfiles/email-setup.el")
    (setq lazy-email-loadedp t)
    (message "Loading email config...done")
    (mu4e)))

(define-key global-map (kbd "s-m") 'lazy-boot-email)

(setq gc-cons-threshold 800000)
