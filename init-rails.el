(defsubmodule rails-reloaded
	(require 'rails-autoload))

;; rinari-mode-prefixes should be defined after rails-autoload ... since
;; it overrides
(defsubmodule rinari
	(setq rinari-minor-mode-prefixes (list "/")) 
  (require 'rinari)
  (dolist (hook '(nxml-mode-hook haml-mode-hook sass-mode-hook magit-mode-hook))
    (add-hook hook (lambda () (rinari-launch)))))

; (defun update-rails-ctags ()
;   (interactive)
;   (let ((default-directory (or (rinari-root) default-directory)))
;     (shell-command (concat "ctags -a -e -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))


;; define quick test bindings
(defun rspec-evil-bindings ()
  (define-key evil-normal-state-local-map ";vv" 'rspec-verify)
  (define-key evil-normal-state-local-map ";va" 'rspec-verify-all)
  (define-key evil-normal-state-local-map ";vs" 'rspec-verify-single))

(add-hook 'rspec-mode-hook 'rspec-evil-bindings)

;; require autotest functionality
;;(require 'autotest)

(provide 'init-rails)
