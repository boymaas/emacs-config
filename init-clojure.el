(require 'clojure-mode)
(require 'clojure-test-mode)

(autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
(autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)
(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

(add-hook 'clojure-mode-hook (lambda ()
                               (enable-paredit clojure-mode-map)
                               ;; don't want this is still defined in other buffers as well
                               ;; (define-key viper-vi-local-user-map ";c" 'slime-compile-and-load-file)
                               ;; (define-key viper-vi-local-user-map ";c" 'slime-compile-and-load-file)
                               (linum-mode nil)))

(defun clojure-cheat ()
  "Opens cheatsheet using org-mode function"
  (interactive)
  (org-open-file "~/Personal/Planning/data/clojure-cheat-sheet-a4.pdf"))

(defun clojure-guide ()
  "Opens programming using org-mode function"
  (interactive)
  (org-open-file "~/Personal/Planning/data/Pragmatic.Bookshelf.Programming.Clojure.May.2009.pdf"))


(provide 'init-clojure)
