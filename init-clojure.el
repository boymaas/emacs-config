(require 'clojure-mode)
;;(require 'swank-clojure-autoload)

(setq clojure-src-root (expand-file-name "~/Projects/clojure-src"))

(eval-after-load 'clojure-mode '(clojure-slime-config))

(add-hook 'clojure-mode-hook (lambda ()
                               (enable-paredit clojure-mode-map)
                               (linum-mode)))

(defun slime-clojure ()
  "Fire up slime running the swank-clojure backend"
  (interactive)
  (slime 'clojure))

(defun clojure-cheat ()
  "Opens cheatsheet using org-mode function"
  (interactive)
  (org-open-file "~/Planning/data/50/e18f6a-7ae2-4086-8d1a-0f30cd18bbee/clojure-cheat-sheet-a4.pdf"))


(defun clojure-project (path)
  "Setup classpaths for a clojure project and starts a new SLIME session."
  (interactive (list
                (ido-read-directory-name
                 "Project root: "
                 (locate-dominating-file default-directory "pom.xml"))))
  (when (get-buffer "*inferior-lisp*")
    (kill-buffer "*inferior-lisp*"))
  (defvar swank-clojure-extra-vm-args nil)
  (defvar slime-lisp-implementations nil)
  (add-to-list 'swank-clojure-extra-vm-args
               (format "-Dclojure.compile.path=%s"
                       (expand-file-name "target/classes/" path)))
  (setq swank-clojure-binary nil
        swank-clojure-jar-path (expand-file-name "target/dependency/" path)
        swank-clojure-extra-classpaths
        (append (mapcar (lambda (d) (expand-file-name d path))
                        '("." "src/" "target/classes/" "test/"))
                (let ((lib (expand-file-name "lib" path)))
                  (if (file-exists-p lib)
                      (directory-files lib t ".jar$"))))
        slime-lisp-implementations
        (cons `(clojure ,(swank-clojure-cmd) :init swank-clojure-init)
              (remove-if #'(lambda (x) (eq (car x) 'clojure))
                         slime-lisp-implementations)))
  (save-window-excursion
    (slime)))


(eval-after-load 'slime-repl-mode
  '(lambda () 
;; want window movement keys working also in slime mode
     ;;(define-key slime-repl-mode-map (kbd "C-<up>") 'windmove-up)
     ;;(define-key slime-repl-mode-map (kbd "C-<down>") 'windmove-down)
     (define-key slime-repl-mode-map (kbd "C-<up>") 'slime-repl-previous-input)
     (define-key slime-repl-mode-map (kbd "C-<down>") 'slime-repl-next-input)))


(global-set-key [f4] 'slime-selector)

(provide 'init-clojure)
