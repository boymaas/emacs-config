;;----------------------------------------------------------------------------
;; Augment search path for external programs (for OSX)
;;----------------------------------------------------------------------------
(when *is-a-mac*
  (eval-after-load "woman"
    '(setq woman-manpath (append (list "/opt/local/man") woman-manpath)))
  (dolist (dir (mapcar 'expand-file-name '("/usr/local/bin" "/opt/local/bin"
					   "/opt/local/lib/postgresql83/bin" "~/bin")))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;;----------------------------------------------------------------------------
;; OS X usability tweaks
;;----------------------------------------------------------------------------
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(0.001))
  (when *is-cocoa-emacs*
    ;; Woohoo!!
    (global-set-key (kbd "M-`") 'ns-next-frame)
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
    (global-set-key (kbd "M-ˍ") 'ns-do-hide-others)  ;; what describe-key reports
    (global-set-key (kbd "M-c") 'ns-copy-including-secondary)
    (global-set-key (kbd "M-v") 'ns-paste-secondary))
  ;; Use Apple-w to close current buffer on OS-X (is normally bound to kill-ring-save)
  (global-set-key [(meta w)] 'kill-this-buffer))

(provide 'init-setup-mac-specific)