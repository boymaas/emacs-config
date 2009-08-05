(load-library "haskell-site-file")

(load-library "cabal-mode")

(require 'hoogle)
(require 'hs-lint)

(setq haskell-program-name (executable-find "ghci"))
(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook
	  (lambda ()
	    (define-key haskell-mode-map "\C-ch" 'hoogle-lookup)
	    (define-key haskell-mode-map "\C-cl" 'hs-lint)
	    (turn-on-haskell-doc-mode)
	    (turn-on-haskell-indent)))

(provide 'init-haskell)
