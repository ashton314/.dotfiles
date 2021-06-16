;; (setq org-priority-highest org-highest-priority)
;; (setq org-priority-lowest org-lowest-priority)

;; Make return key smarter when dealing with lists
(defun org-return--around (old-fn &rest args)
  (let ((context (org-element-lineage (org-element-at-point) '(item))))
    (if (and context (not args))
        (org-insert-item (org-element-property :checkbox context))
      (apply old-fn args))))

(advice-add 'org-return :around 'org-return--around)
