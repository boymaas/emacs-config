;; See http://bc.tech.coop/blog/070927.html
(setq slime-lisp-implementations
      '((sbcl ("sbcl") :coding-system utf-8-unix)
        (cmucl ("cmucl") :coding-system iso-latin-1-unix)))
(require 'slime-autoloads)
(add-auto-mode 'lisp-mode "\\.cl$")
(global-set-key [f4] 'slime-selector)
(add-hook 'lisp-mode-hook (lambda ()
                            (cond ((not (featurep 'slime))
                                   (require 'slime)
                                   (normal-mode)))))

(eval-after-load "slime"
  '(progn
     (add-to-list 'load-path (concat (directory-of-library "slime") "/contrib"))
     (slime-setup '(slime-fancy slime-banner slime-asdf))
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (add-hook 'slime-mode-hook 'pretty-lambdas)
     (add-hook 'slime-mode-hook (lambda () (enable-paredit slime-mode-map)))
     (slime-setup)))

;; From http://bc.tech.coop/blog/070515.html
(defun lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol currently under the curser"
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

(define-key lisp-mode-map (kbd "C-c l") 'lispdoc)

(autoload 'redshank-mode "redshank" "Minor mode for editing and refactoring (Common) Lisp code." t)
(autoload 'turn-on-redshank-mode "redshank" "Turn on Redshank mode. Please see function `redshank-mode'." t)
(add-hook 'lisp-mode-hook 'turn-on-redshank-mode)


(provide 'init-common-lisp)
