(require 'rails-autoload)
(require 'rinari)
(dolist (hook '(nxml-mode-hook haml-mode-hook sass-mode-hook magit-mode-hook))
  (add-hook hook (lambda () (rinari-launch))))

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command (concat "ctags -a -e -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))


;(add-hook 'rails-minor-mode-hook (lambda () (local-set-key [f6] 'recompile)))

;; define quick test bindings
(defun rspec-evil-bindings ()
  (define-key evil-normal-state-local-map ";vv" 'rspec-verify)
  (define-key evil-normal-state-local-map ";va" 'rspec-verify-all)
  (define-key evil-normal-state-local-map ";vs" 'rspec-verify-single))

(add-hook 'rspec-mode-hook 'rspec-evil-bindings)

;; require autotest functionality
;;(setq load-path (cons (expand-file-name "~/.emacs.d/site-lisp/ruby-rails/")  load-path))
(require 'autotest)

(provide 'init-rails)
