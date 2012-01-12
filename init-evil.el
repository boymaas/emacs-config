;; require the vim evil :-)
(require 'evil)
(evil-mode 1)

(setq evil-shift-width 2)

;; use surround.vim but then for emacs ..
(require 'surround)
(global-surround-mode 1)

;; some modes aren't meant for evil
(dolist (m '(el-get-package-menu-mode git-status-mode grep-mode))
	(add-to-list 'evil-emacs-state-modes m))

;; Remap org-mode meta keys for convenience
(mapcar (lambda (evil-state)
	  (evil-declare-key evil-state org-mode-map
	    (kbd "M-l") 'org-metaright
	    (kbd "M-h") 'org-metaleft
	    (kbd "M-k") 'org-metaup
	    (kbd "M-j") 'org-metadown
	    (kbd "M-L") 'org-shiftmetaright
	    (kbd "M-H") 'org-shiftmetaleft
	    (kbd "M-K") 'org-shiftmetaup
	    (kbd "M-J") 'org-shiftmetadown))
	'(normal insert))

(define-key evil-normal-state-map "g;" 'session-jump-to-last-change)
(define-key evil-normal-state-map ";b" 'ibuffer)
(define-key evil-normal-state-map ";;" 'switch-to-buffer)
(define-key evil-normal-state-map ";'" 'delete-window)
(define-key evil-normal-state-map ";\\" 'delete-other-windows)
(define-key evil-normal-state-map ";o" 'other-window)
(define-key evil-normal-state-map ";d" 'dired)
(define-key evil-normal-state-map ";f" 'ido-find-file)
(define-key evil-normal-state-map ";r" 'steve-ido-choose-from-recentf)
(define-key evil-normal-state-map ";x" 'smex)
(define-key evil-visual-state-map ";x" 'smex)
(define-key evil-normal-state-map ";X" 'smex-update-and-run)
(define-key evil-normal-state-map ";a" 'anything)
(define-key evil-normal-state-map ";t" 'ido-find-tag)
(define-key evil-normal-state-map ";p" 'textmate-goto-file)

(provide 'init-evil)