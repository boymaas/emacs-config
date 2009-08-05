(defvar *window-system-color-theme* 'color-theme-pierson
  "Color theme to use in window-system frames")
(defvar *tty-color-theme* 'color-theme-emacs-nw
  "Color theme to use in TTY frames")

(require 'color-theme)
(require 'color-theme-tango-2)
;(require 'color-theme-autoloads)
(eval-after-load "color-theme"
  '(progn
     (color-theme-tango-2)))

;; (color-theme-pierson) ; Light, favourite
;; (color-theme-high-contrast)
;; (color-theme-snowish)
;; (color-theme-marquardt)
;; (color-theme-clarity) ; dark
;; (color-theme-dark-laptop) ; dark
;; (color-theme-billw) ; dark
;; (color-theme-oswald) ; dark
;; (color-theme-zenburn) ; dark, low contrast
;; (color-theme-standard)

(defun apply-best-color-theme-for-frame-type (frame)
  (let ((prev-frame (selected-frame)))
    (select-frame frame)
    (prog1
	(if window-system
	    (preserving-default-font-size
	     (funcall *window-system-color-theme*))
	  (funcall *tty-color-theme*))
      (select-frame prev-frame))))

(defun reapply-color-themes ()
  (interactive)
  (mapcar 'apply-best-color-theme-for-frame-type (frame-list)))

(set-variable 'color-theme-is-global nil)
(add-hook 'after-make-frame-functions 'apply-best-color-theme-for-frame-type)
(apply-best-color-theme-for-frame-type (selected-frame))

(provide 'init-themes)
