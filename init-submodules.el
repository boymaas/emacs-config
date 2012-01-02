;;----------------------------------------------------------------------------
;; Automatically byte-compile .el files
;;----------------------------------------------------------------------------
(defsubmodule byte-code-cache
  (require 'init-byte-code-cache))

;;----------------------------------------------------------------------------
;; Use elisp package manager (http://tromey.com/elpa/)
;;----------------------------------------------------------------------------
(defsubmodule elpa 
  (setq load-path (cons (expand-file-name "~/.emacs.d/elpa") load-path))
  (require 'package)
  (package-initialize))

(defsubmodule setup-mac-specific
  (require 'init-setup-mac-specific))

(defsubmodule xterm-and-console
  (require 'init-xterm-and-console))

(defsubmodule network-location-in-titlebar
  (require 'init-network-location-in-titlebar))

;;----------------------------------------------------------------------------
;; Network proxy configuration
;;----------------------------------------------------------------------------
(defsubmodule proxies
  (require 'init-proxies))

;;----------------------------------------------------------------------------
;; Enhanced dired
;;----------------------------------------------------------------------------
(defsubmodule enhanced-dired
  (require 'dired+)
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file))

;;----------------------------------------------------------------------------
;; VI emulation and related key mappings (EVIL)
;;----------------------------------------------------------------------------
(defsubmodule evil
  (require 'init-evil))

;;----------------------------------------------------------------------------
;; ECB = Emacs Code Browser
;;----------------------------------------------------------------------------
(defsubmodule ecb
  (require 'init-ecb))

;;----------------------------------------------------------------------------
;; Basic flymake configuration
;;----------------------------------------------------------------------------
(defsubmodule flymake
  (require 'init-flymake))

;;----------------------------------------------------------------------------
;; Twitter
;;----------------------------------------------------------------------------
(defsubmodule twitter
  (require 'init-twitter))

;;----------------------------------------------------------------------------
;; Erlang
;;----------------------------------------------------------------------------
(defsubmodule erlang
  ;;(setq load-path (cons (expand-file-name "/usr/local/share/emacs/site-lisp/distel") load-path))
  ;;(defun my-erlang-load-hook ()
  ;; (setq erlang-root-dir "/opt/otp/lib/erlang"))
  ;;(add-hook 'erlang-load-hook 'my-erlang-load-hook)
  (setq erlang-root-dir "/opt/local/lib/erlang")
  (require 'erlang-start))
  ;;(require 'distel)
  ;;(add-hook 'erlang-mode-hook 'distel-erlang-mode-hook))

;;----------------------------------------------------------------------------
;; Javascript
;;----------------------------------------------------------------------------
(defsubmodule javascript
  (require 'init-javascript))

;;----------------------------------------------------------------------------
;; Subversion
;;----------------------------------------------------------------------------
(defsubmodule svn
  (require 'psvn))

;;----------------------------------------------------------------------------
;; Darcs
;;----------------------------------------------------------------------------
(defsubmodule darcs
  (require 'init-darcs))

;;----------------------------------------------------------------------------
;; Git
;;----------------------------------------------------------------------------
(defsubmodule git
  (require 'init-git))

;;----------------------------------------------------------------------------
;; File and buffer navigation
;;----------------------------------------------------------------------------
(defsubmodule file-and-buffer-navigation
  (require 'recentf)
  (setq recentf-max-saved-items 100)
  (require 'init-ido)
  (require 'init-anything)
  (require 'textmate)
    (smex-initialize))

;;----------------------------------------------------------------------------
;; Tags
;;----------------------------------------------------------------------------
(defsubmodule etags
  (require 'init-etags))

;;----------------------------------------------------------------------------
;; Autocomplete
;;----------------------------------------------------------------------------
(defsubmodule auto-complete
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict/")
  (ac-config-default)
  (global-auto-complete-mode t)
    ;(ac-set-trigger-key "TAB")
  (setq ac-auto-start 2)
  (define-key ac-mode-map (kbd "M-/") 'auto-complete)
  (setq ac-dwim nil)

  ;; This stops "end" followed by "RET" getting completed to something
  ;; like "endomorph" - have to use an explicit "TAB" to complete.
  (define-key ac-complete-mode-map (kbd "\r") nil))

;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defsubmodule window-split
  (require 'init-window-split))


;;----------------------------------------------------------------------------
;; Desktop saving
;;----------------------------------------------------------------------------
;; save a list of open files in ~/.emacs.d/.emacs.desktop
;; save the desktop file automatically if it already exists
(defsubmodule desktop-and-session-saving
  (setq desktop-path '("~/.emacs.d"))
  (setq desktop-save 'if-exists)
  (desktop-save-mode 1)


  (autoload 'save-current-configuration "revive" "Save status" t)
  (autoload 'resume "revive" "Resume Emacs" t)
  (autoload 'wipe "revive" "Wipe Emacs" t)
  (define-key ctl-x-map "S" 'save-current-configuration)
  (define-key ctl-x-map "F" 'resume)
  (define-key ctl-x-map "K" 'wipe)


;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
  (require 'session)
  (setq session-save-file (expand-file-name "~/.emacs.d/.session"))
  (add-hook 'after-init-hook 'session-initialize)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
  (setq desktop-globals-to-save
	(append '((extended-command-history . 30)
		  (file-name-history        . 100)
		  (ido-last-directory-list  . 100)
		  (ido-work-directory-list  . 100)
		  (ido-work-file-list       . 100)
		  (grep-history             . 30)
		  (compile-history          . 30)
		  (minibuffer-history       . 50)
		  (query-replace-history    . 60)
		  (read-expression-history  . 60)
		  (regexp-history           . 60)
		  (regexp-search-ring       . 20)
		  (search-ring              . 20)
		  (shell-command-history    . 50)
		  tags-file-name
		  register-alist))))

(defsubmodule maxframe
  (require 'init-maxframe))

;;----------------------------------------------------------------------------
;; Fonts
;;----------------------------------------------------------------------------
(defsubmodule fonts
  (require 'init-fonts))

;;----------------------------------------------------------------------------
;; Color themes
;;----------------------------------------------------------------------------
(defsubmodule colorthemes
  (require 'init-themes))

;;----------------------------------------------------------------------------
;; Eproject
;;----------------------------------------------------------------------------
(defsubmodule eproject
  (require 'eproject)
  (require 'eproject-extras))

;;----------------------------------------------------------------------------
;; Org-mode
;;----------------------------------------------------------------------------
(defsubmodule org-mode
  (require 'init-org))

;;----------------------------------------------------------------------------
;; NXML
;;----------------------------------------------------------------------------
(defsubmodule nxml
  (require 'init-nxml))

;;----------------------------------------------------------------------------
;; Python
;;----------------------------------------------------------------------------
(defsubmodule python
  (require 'init-python-mode))

;;----------------------------------------------------------------------------
;; Ruby & Rails
;;----------------------------------------------------------------------------
(defsubmodule ruby
  (require 'init-ruby-mode))

(defsubmodule rails
  (require 'init-rails))

;;----------------------------------------------------------------------------
;; CSS mode
;;----------------------------------------------------------------------------
(defsubmodule css
  (require 'init-css))

;;----------------------------------------------------------------------------
;; YAML mode
;;----------------------------------------------------------------------------
(defsubmodule yaml
  (autoload 'yaml-mode "yaml-mode" "Mode for editing YAML files" t))


;;----------------------------------------------------------------------------
;; CSV mode and csv-nav mode
;;----------------------------------------------------------------------------
(defsubmodule csv
  (autoload 'csv-mode "csv-mode" "Major mode for editing comma-separated value files." t)
  (autoload 'csv-nav-mode "csv-nav-mode" "Major mode for navigating comma-separated value files." t))

;;----------------------------------------------------------------------------
;; Shell mode
;;----------------------------------------------------------------------------
(defsubmodule shell
  (autoload 'flymake-shell-load "flymake-shell" "On-the-fly syntax checking of shell scripts" t)
  (add-hook 'sh-mode-hook 'flymake-shell-load))


;;----------------------------------------------------------------------------
;; Lisp / Scheme / Slime
;;----------------------------------------------------------------------------
(defsubmodule lisp
  (require 'init-lisp))
(defsubmodule common-lisp
  (require 'init-common-lisp))
(defsubmodule clojure
  (require 'init-clojure))
(defsubmodule scheme
  ; See http://bc.tech.coop/scheme/scheme-emacs.htm
  (require 'quack))

;;----------------------------------------------------------------------------
;; Haskell
;;----------------------------------------------------------------------------
(defsubmodule haskell
  (require 'init-haskell))

;;----------------------------------------------------------------------------
;; OCaml
;;----------------------------------------------------------------------------
(defsubmodule ocaml
  (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t))

;;----------------------------------------------------------------------------
;; Gnus emailclient from emacs
;;----------------------------------------------------------------------------
(defsubmodule gnus
  (require 'init-gnus))

;;----------------------------------------------------------------------------
;; Add spell-checking in comments for all programming language modes
;;----------------------------------------------------------------------------
(defsubmodule spell-check
  (require 'init-flyspell))

(defsubmodule coffeescript
  (require 'init-coffeescript))

;;----------------------------------------------------------------------------
;; Haml-mode / Sass-mode
;;----------------------------------------------------------------------------
(defsubmodule haml
  (require 'haml-mode))
(defsubmodule sass
  (require 'sass-mode))

(provide 'init-submodules)