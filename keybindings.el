(define-key global-map (kbd "s-<return>") 'toggle-frame-fullscreen)
(define-key global-map (kbd "s-u") 'toggle-frame-transparency)
(define-key global-map (kbd "s-}") 'tab-next)
(define-key global-map (kbd "s-{") 'tab-previous)
(define-key global-map (kbd "s-t") 'tab-new)

(defvar transparency--toggle-var t)
(defun toggle-frame-transparency ()
  "Toggle the transparency of all frames."
  (interactive)
  (if transparency--toggle-var
      (set-frame-parameter (selected-frame) 'alpha '(83 83))
    (set-frame-parameter (selected-frame) 'alpha '(100 100)))
  (setq transparency--toggle-var (not transparency--toggle-var)))
