;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor enhancements to M-x					   ;;
;; 								   ;;
;; This uses Selectrum, but also displays the key the command is   ;;
;; bound to, as well as colorizes the currently active major/minor ;;
;; modes.							   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar command-keybinding-hash (make-hash-table) "Cache of commands to the keys they are bound to.")

(defun rehash-key-bindings ()
  "Rebuild `command-keybinding-hash' so that calling
\[execute-extended-command] will show the proper keybindings next
to the functions they're bound to."
  (interactive)
  (obarray-map
   (lambda (sym)
     (when (and (commandp sym) (where-is-internal sym))
       (puthash sym (format "%s%s" (symbol-name sym)
			    (keybind-propertize (key-description (car (where-is-internal sym)))))
		command-keybinding-hash)
       ))
   obarray)
  (message "Finished building keybinding cache"))

(defun keybind-propertize (str)
  (format " (%s)" (propertize str 'face 'font-lock-doc-face)))

(defun guilded-mx ()
  "Like `execute-extended-command', but with fancy annotations."
  (interactive)
  (command-execute
   (intern				; This strips off the annotations since they're all just propertized on
    (completing-read
     "M-x "
     (let ((modes (make-hash-table :test #'equal))
	   (keymap (make-hash-table :test #'equal))
           (cmds nil))
       (map-do
	(lambda (var _)
          (when (boundp var)
            (puthash
             (or (get var :minor-mode-function) var)
             (propertize
              (symbol-name var)
              'face
              (if (symbol-value var)
                  'compilation-mode-line-exit
		'compilation-mode-line-fail))
             modes)))
	minor-mode-alist)

       (obarray-map
	(lambda (sym)
          (when (commandp sym)
            (push
             (or (gethash sym modes)
		 (gethash sym command-keybinding-hash)
		 (symbol-name sym))
             cmds)))
	obarray)
       cmds)
     nil
     'require-match))
   'record))

(define-key global-map (kbd "M-x") 'guilded-mx)

(add-hook 'after-init-hook 'rehash-key-bindings)

