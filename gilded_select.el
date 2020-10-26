;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gilded-Mx: Minor enhancements to M-x				   ;;
;; 								   ;;
;; This uses Selectrum, but also displays the key the command is   ;;
;; bound to, as well as colorizes the currently active major/minor ;;
;; modes.							   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gilded-command-keybinding-hash (make-hash-table) "Cache of commands to the keys they are bound to.")

(defun gilded-rehash-key-bindings ()
  "Rebuild `gilded-command-keybinding-hash' so that calling
`execute-extended-command' will show the proper keybindings next
to the functions they're bound to."
  (interactive)
  (obarray-map
   (lambda (sym)
     (when (and (commandp sym) (where-is-internal sym))
       (puthash sym (format "%s %s" (symbol-name sym)
			    (keybind-propertize (key-description (car (where-is-internal sym)))))
		gilded-command-keybinding-hash)
       ))
   obarray)
  (message "Finished building keybinding cache"))

(defun keybind-propertize (str)
  "Function used to format the keybinding annotation. Receives
the keybinding without any frills."
  (format "(%s)" (propertize str 'face 'font-lock-doc-face)))

(defun string-gen-times (n str acc)
  (if (= n 0) acc (string-gen-times (- n 1) str (cons str acc))))

(defun gilded-prompt-string (prefix)
  "Given a prefix argument, builds a prompt string for `gilded-mx'."
  (format "%sM-x "
	  (if (and prefix (listp prefix) (not (= (car prefix) 0)) (memq (car prefix) '(4 16 32 128)))
	      (apply #'concat (string-gen-times (log (car prefix) 4) "C-u " '()))
	    (if prefix (format "(%s) " prefix) ""))))

(defun gilded-mx (prefix)
  "Like `execute-extended-command', but with fancy annotations."
  (interactive "P")
  (let ((current-prefix-arg prefix))
    (call-interactively
     (intern				; This strips off the annotations since they're all just propertized on
      (car (split-string			; Remove the keybind annotation if present
	    (completing-read
	     (gilded-prompt-string prefix)
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
			 (gethash sym gilded-command-keybinding-hash)
			 (symbol-name sym))
		     cmds)))
		obarray)
	       cmds)
	     nil
	     'require-match) " ")))
     'record)))

;; Rehash all the keybindings when we change to a new mode
;; (add-hook 'after-change-major-mode-hook 'gilded-rehash-key-bindings)
