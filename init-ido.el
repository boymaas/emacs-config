;; some useful keybindings
(add-hook 'ido-setup-hook 'ido-my-keys)
(defun ido-my-keys ()
  (define-key ido-completion-map "\C-w" 'ido-delete-backward-word-updir))

;; Use C-f during file selection to switch to regular find-file
(ido-mode t)  ; use 'buffer rather than t to use only buffer switching
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)

(defun steve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open recent file: " recentf-list nil t)))
(global-set-key [(meta f11)] 'steve-ido-choose-from-recentf)

;; ido tag completion
(defun ido-find-tag ()
    "Find a tag using ido"
    (interactive)
    (tags-completion-table)
    (let (tag-names)
      (mapc (lambda (x)
              (unless (integerp x)
                (push (prin1-to-string x t) tag-names)))
            tags-completion-table)
      (find-tag (ido-completing-read "Tag: " tag-names))))


;;----------------------------------------------------
;; smex better M-x completion, uses most recently used first
(require 'smex)
;;(smex-initialize) ;; do this in initfile at the end
(setq smex-save-file "~/.emacs.d/smex.save")
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(provide 'init-ido)
