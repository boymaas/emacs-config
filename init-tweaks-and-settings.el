;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq default-indicate-empty-lines t)

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(winner-mode 1)
(windmove-default-keybindings 'shift)

;;----------------------------------------------------------------------------
;; Make yes-or-no questions answerable with 'y' or 'n'
;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)

;;----------------------------------------------------------------------------
;; To be able to M-x without meta
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;;  -------------------------------------------
;;  Highlight the line we are currently editing
(global-hl-line-mode t)

;;----------------------------------------------------------------------------
;; Highlight URLs in comments/strings
;;----------------------------------------------------------------------------
;;(add-hook 'find-file-hooks 'goto-address-prog-mode)

;;----------------------------------------------------------------------------
;; Use regex searches by default.
;;----------------------------------------------------------------------------
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;;----------------------------------------------------------------------------
;; Modeline tweaks
;;----------------------------------------------------------------------------
(size-indication-mode)

;;----------------------------------------------------------------------------
;; Modeline tweaks
;;----------------------------------------------------------------------------
(autoload 'linum-mode "linum" "Toggle line numbering" t)

;;----------------------------------------------------------------------------
;; Scroll the window smoothly with the up/down arrows
;;----------------------------------------------------------------------------
(require 'smooth-scrolling)
(setq scroll-preserve-screen-position t)

;;----------------------------------------------------------------------------
;; Nicer naming of buffers for files with identical names
;;----------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;----------------------------------------------------------------------------
;; Use ibuffer instead of the built in buffer list
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f2>") 'ibuffer)

;;----------------------------------------------------------------------------
;; Dynamic expansion tweaks
;;----------------------------------------------------------------------------
(eval-after-load "hippie-exp"
  '(setq hippie-expand-try-functions-list
	 (remove 'try-expand-line hippie-expand-try-functions-list)))

;;----------------------------------------------------------------------------
;; Log typed commands into a buffer for demo purposes
;;----------------------------------------------------------------------------
(autoload 'mwe:log-keyboard-commands "mwe-log-commands"
  "Log commands executed in the current buffer" t)

;;----------------------------------------------------------------------------
;; Show and edit all lines matching a regex
;;----------------------------------------------------------------------------
(require 'all)

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
;;(tool-bar-mode -1)
;;(scroll-bar-mode -1)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Visible bell
(setq visible-bell 1)

;;----------------------------------------------------------------------------
; Automatically set execute perms on files if first line begins with '#!'
;;----------------------------------------------------------------------------
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;----------------------------------------------------------------------------
;; Conversion of line endings
;;----------------------------------------------------------------------------
;; Can also use "C-x ENTER f dos" / "C-x ENTER f unix" (set-buffer-file-coding-system)
(require 'eol-conversion)

(provide 'init-tweaks-and-settings)