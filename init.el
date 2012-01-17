;; -*- coding: utf-8 -*-

;; Get some context awareness
(setq *macbook-pro-support-enabled* nil) ;; init-maxframe

(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

;;----------------------------------------------------------------------------
;; Make elisp more civilised
;;----------------------------------------------------------------------------
(require 'cl)

;;----------------------------------------------------------------------------
;; Set load path
;;----------------------------------------------------------------------------
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
	   (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        ;;(setq load-path my-lisp-dir)
	(normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;;----------------------------------------------------------------------------
;; Load our utility functions, most importantly the defsubmodule
;;----------------------------------------------------------------------------
(require 'init-utility-functions)

;;----------------------------------------------------------------------------
;; Define all submodules, so we can enable / disable them by hand or on demand
;;----------------------------------------------------------------------------
(require 'init-submodules)

;;----------------------------------------------------------------------------
;; Define autolaods, as to delay loading of submodules only when needed.
;;----------------------------------------------------------------------------
(require 'init-autoloads)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(defsubmodule emacs-server
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;----------------------------------------------------------------------------
;; Setup default locale and everything else
;;----------------------------------------------------------------------------
(require 'init-locales)

;; ----------------------------------------------------------------------------
;; General usability tweaks, small settings that make my life easier
;; ----------------------------------------------------------------------------
(require 'init-tweaks-and-settings)
(require 'init-auto-mode-settings)

;;----------------------------------------------------------------------------
;; Enable bare minimum functionality
;;----------------------------------------------------------------------------
(module-enable-setup-mac-specific)    ;; need this for load paths executables and manfiles
(module-enable-elpa)                  ;; some packages are installed via the package manager
(module-enable-evil)                  ;; VIM bindings
(module-enable-auto-complete)         ;; save my knuckles and gather information
(module-enable-git)                   ;; I wanna control ...
(module-enable-enhanced-dired)        ;; better dir-editor
(module-enable-flymake)               ;; on the fly syntax checking ..

(module-enable-org-mode)              ;; where would I be without it :-D

(module-enable-lisp)                  ;; generic lisp functionality, paredit etc

;;----------------------------------------------------------------------------
;; Enable frequently used functionality
;;----------------------------------------------------------------------------
;(module-enable-ecb)
(module-enable-ruby)
(module-enable-rails)
(module-enable-rinari)
(module-enable-haml)
(module-enable-sass)
(module-enable-yaml)
(module-enable-coffeescript)
(module-enable-javascript)
;; For other modules check init-modules ... all are defined there

;;----------------------------------------------------------------------------
;; optional modules
;;----------------------------------------------------------------------------
(module-enable-etags)

;; ----------------------------------------------------------------------------
;; Include command line completion of M-x using smex
;; needs to be initialized at the end. Since it indexes all symbols which
;; are required in the different submodules
;; ----------------------------------------------------------------------------
(module-enable-file-and-buffer-navigation)

;; ----------------------------------------------------------------------------
;; Let's not forget our looks
;; ----------------------------------------------------------------------------
;(module-enable-colorthemes)
(module-enable-maxframe)

;; ----------------------------------------------------------------------------
;; And restore our context 
;; ----------------------------------------------------------------------------
;(module-enable-desktop-and-session-saving)
