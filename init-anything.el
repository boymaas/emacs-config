(require 'anything)
(require 'anything-config)

(setq anything-sources
      '(
	anything-c-source-recentf
	anything-c-source-lacarte
	
	;anything-c-source-etags-select
        ;anything-c-source-org-headline
        ;anything-c-source-buffers
        ;anything-c-source-minibuffer-history
	))

(setq anything-samewindow t)
(setq anything-input-idle-delay 0.2)

;(add-to-list 'anything-c-ctags-modes 'ruby-mode)

;(global-set-key [\M-f10] 'anything-at-point) ;; With C-u prefix, starts with symbol at point


(provide 'init-anything)
