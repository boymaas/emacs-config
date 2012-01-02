;;----------------------------------------------------------------------------
;; Coffee script
;;----------------------------------------------------------------------------
(require 'coffee-mode)

(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2)
 (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
 (define-key coffee-mode-map [(meta R)] 'coffee-compile-region))

(add-hook 'coffee-mode-hook
  '(lambda()
     (coffee-custom)))

(provide 'init-coffeescript)