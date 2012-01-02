;;----------------------------------------------------------------------------
;; Add hooks to allow conditional setup of window-system and console frames
;;----------------------------------------------------------------------------
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Selectively run either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (select-frame frame)
  (run-hooks (if window-system
		 'after-make-window-system-frame-hooks
	       'after-make-console-frame-hooks)))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)


;;----------------------------------------------------------------------------
;; Console-specific set-up
;;----------------------------------------------------------------------------
(defun fix-up-xterm-control-arrows ()
  (define-key function-key-map "\e[1;5A" [C-up])
  (define-key function-key-map "\e[1;5B" [C-down])
  (define-key function-key-map "\e[1;5C" [C-right])
  (define-key function-key-map "\e[1;5D" [C-left])
  (define-key function-key-map "\e[5A"   [C-up])
  (define-key function-key-map "\e[5B"   [C-down])
  (define-key function-key-map "\e[5C"   [C-right])
  (define-key function-key-map "\e[5D"   [C-left]))

(add-hook 'after-make-console-frame-hooks
	  (lambda ()
	    (fix-up-xterm-control-arrows)
	    (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
	    (mwheel-install)))

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (let ((prev-frame (selected-frame)))
	      (select-frame frame)
	      (prog1
		  (unless window-system
		    (set-frame-parameter frame 'menu-bar-lines 0))
		(select-frame prev-frame)))))

(provide 'init-xterm-and-console)