(require 'rails-autoload)
(require 'rinari)
(dolist (hook '(nxml-mode-hook haml-mode-hook sass-mode-hook magit-mode-hook))
  (add-hook hook (lambda () (rinari-launch))))

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command (concat "ctags -a -e -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))


;(add-hook 'rails-minor-mode-hook (lambda () (local-set-key [f6] 'recompile)))

(provide 'init-rails)
