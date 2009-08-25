(require 'clojure-mode)
(require 'swank-clojure-autoload)

(setq clojure-src-root (expand-file-name "~/Projects/clojure-src"))
(setq swank-clojure-jar-path (expand-file-name  "~/Projects/clojure/clojure-1.0.0.jar"))

(eval-after-load 'clojure-mode '(clojure-slime-config))

(defun slime-clojure ()
  "Fire up slime running the swank-clojure backend"
  (interactive)
  (slime 'clojure))

(global-set-key [f4] 'slime-selector)

(provide 'init-clojure)
