(autoload 'python-mode "python-mode" "Python Mode." t)  
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))  
(add-to-list 'interpreter-mode-alist '("python" . python-mode))  
(require 'python-mode)  

;; Python stuff for outline mode.
;;(defvar py-outline-regexp "^\\([ \t]*\\)\\(def\\|class\\|if\\|elif\\|else\\|while\\|for\\|try\\|except\\|with\\)"
;;  "This variable defines what constitutes a 'headline' to outline mode.")
(defvar py-outline-regexp "^\\([ \t]*\\)\\(def\\|class\\|@\\)"
  "This variable defines what constitutes a 'headline' to outline mode.")

(defun py-outline-level ()
  "Report outline level for Python outlining."
  (save-excursion
    (end-of-line)
    (let ((indentation (progn
                         (re-search-backward py-outline-regexp)
                         (match-string-no-properties 1))))
      (if (and (> (length indentation) 0)
               (string= "\t" (substring indentation 0 1)))
          (length indentation)
        (/ (length indentation) py-indent-offset)))))

(add-hook 'python-mode-hook  
          (lambda ()  
            (set-variable 'py-indent-offset 4)  
            ;;(set-variable 'py-smart-indentation nil)  
            (set-variable 'indent-tabs-mode nil)  
            (define-key py-mode-map (kbd "RET") 'newline-and-indent)  
            ;;(define-key py-mode-map [tab] 'yas/expand)  
            ;;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)  
            ;;(smart-operator-mode-on)  
            (linum-mode)
            
            (outline-minor-mode 1)
            (setq outline-regexp py-outline-regexp
                  outline-level 'py-outline-level)
            (hide-body)

            (define-key viper-vi-local-user-map "zo" 'show-subtree)
            (define-key viper-vi-local-user-map "zc" 'hide-leaves)
            ))
 
(defun py-open-block ()
  (interactive)
  (outline-up-heading)
  (show-subtree))

(defun py-close-block ()
  (interactive)
  (outline-up-heading)
  (hide-leaves))

(when *python-ropemacs-support-enabled*
  ;; pymacs  
  (autoload 'pymacs-apply "pymacs")  
  (autoload 'pymacs-call "pymacs")  
  (autoload 'pymacs-eval "pymacs" nil t)  
  (autoload 'pymacs-exec "pymacs" nil t)  
  (autoload 'pymacs-load "pymacs" nil t)  
  ;;(eval-after-load "pymacs"  
  ;;  '(add-to-list 'pymacs-load-path "~/.emacs.d/site-lisp/pymacs/python-site-dir"))
  (pymacs-load "ropemacs" "rope-"))  
  
(setq ropemacs-enable-autoimport t)  

;; Auto completeion for python-mode
(when *python-ropemacs-support-enabled*
  (require 'auto-complete-python)
  (ac-ropemacs-init))

(require 'auto-complete-yasnippet)
(add-hook 'python-mode-hook  
          (lambda ()  
            (setq ac-sources (append (list 'ac-source-yasnippet) ac-sources))))
            

;; Auto Syntax Error Hightlight  
(when (load "flymake" t)  
  (defun flymake-pyflakes-init ()  
    (let* ((temp-file (flymake-init-create-temp-buffer-copy  
                       'flymake-create-temp-inplace))  
           (local-file (file-relative-name  
                        temp-file  
                        (file-name-directory buffer-file-name))))  
      (list "pyflakes" (list local-file))))  
  (add-to-list 'flymake-allowed-file-name-masks  
               '("\\.py\\'" flymake-pyflakes-init)))  
;;(add-hook 'find-file-hook 'flymake-find-file-hook)  


;; IPython support
;; use emacs-paster-shell to run paster inside emacs
;; can use this to experiment withing pylons
;; (setq py-python-command "emacs-paster-shell") 


;; make colors suitable for black background
(setq py-python-command-args '("-colors" "Linux"))
(require 'ipython)

;; ipython completion
;(setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('')))\n")
(setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s')))\n")

(provide 'init-python-mode)
