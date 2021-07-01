(setq mu4e-mu-binary "/usr/local/bin/mu")

;; Folders
(setq mu4e-sent-folder "/Sent")
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-trash-folder "/Trash")
(setq mu4e-refile-folder "/Archive")
(setq mu4e-attachment-dir "~/Downloads")

;; Signature stuffs
;; (setq user-full-name "")
;; (setq user-mail-address "")
;; (setq mu4e-compose-signature "")

;; (setq smtpmail-smtp-server "")
;; (setq smtpmail-smtp-user "")
(setq smtpmail-stream-type 'starttls)
(setq smtpmail-smtp-service 587)

(add-to-list 'load-path "/usr/local/Cellar/mu/1.4.15/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)

(setq mu4e-completing-read-function 'completing-read)

;; Org-mode integration
(setq org-mu4e-link-query-in-headers-mode nil)

;; give me ISO(ish) format date-time stamps in the header list
(setq  mu4e-headers-date-format "%Y-%m-%d %H:%M")

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "mbsync -a")

;; Fixes problem with duplicate UID errors (see
;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/)
(setq mu4e-change-filenames-when-moving t)

;; customize the reply-quote-string
;; M-x find-function RET message-citation-line-format for docs
(setq message-citation-line-format "%N on %Y-%m-%d %H:%M %Z:\n")
(setq message-citation-line-function 'message-insert-formatted-citation-line)

;; Turn on word-wrap automatically when viewing emails
(add-hook 'mu4e-view-mode-hook (lambda () (visual-line-mode)))

;; Don't hard-wrap my emails as I write!
(add-hook 'mu4e-compose-mode-hook (lambda () (auto-fill-mode -1)))

;; How to send messages
(setq message-send-mail-function 'smtpmail-send-it)

(define-key global-map (kbd "s-m") 'mu4e)
