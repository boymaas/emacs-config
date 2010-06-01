(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(blink-cursor-delay 0)
 '(blink-cursor-interval 0.4)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-firefox-arguments (quote ("--enable-plugins")))
 '(browse-url-firefox-program "chromium-browser")
 '(buffers-menu-max-size 30)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(csv-separators (quote ("," ";" "|" "	")))
 '(current-language-environment "UTF-8")
 '(default-input-method "latin-1-prefix")
 '(desktop-save t)
 '(ecb-compile-window-height 0.2)
 '(ecb-create-layout-file "~/.emacs.d/.ecb-user-layouts.el")
 '(ecb-grep-find-function (quote rgrep))
 '(ecb-layout-name "right-nav")
 '(ecb-layout-window-sizes (quote (("left-analyse" (0.20297029702970298 . 0.33962264150943394) (0.20297029702970298 . 0.20754716981132076) (0.20297029702970298 . 0.20754716981132076) (0.20297029702970298 . 0.22641509433962265)))))
 '(ecb-options-version "2.32")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-process-non-semantic-files nil)
 '(ecb-run-ediff-in-ecb-frame nil)
 '(ecb-tip-of-the-day nil)
 '(ecb-wget-setup (quote ("wget" . other)))
 '(ecb-windows-width 0.2)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(european-calendar-style t)
 '(face-default-stipple "gray3" t)
 '(follow-auto nil)
 '(frame-background-mode nil)
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode nil)
 '(global-smart-tab-mode nil)
 '(grep-highlight-matches t)
 '(grep-scroll-output t)
 '(highlight-changes-global-changes-existing-buffers t)
 '(highlight-changes-global-initial-state (quote active))
 '(ibuffer-enable t)
 '(ido-use-faces t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(list-directory-verbose-switches "-lh")
 '(make-backup-files nil)
 '(max-lisp-eval-depth 800)
 '(mouse-yank-at-point t)
 '(newsticker-url-list-defaults (quote (("Emacs Wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss" nil 3600) ("Quote of the day" "http://www.quotationspage.com/data/qotd.rss" "07:00" 86400) ("The Register" "http://www.theregister.co.uk/tonys/slashdot.rdf") ("slashdot" "http://slashdot.org/index.rss" nil 3600))))
 '(org-agenda-files (quote ("~/Planning/innovita/het-schouwtje.gtd" "~/Planning/concepts/ocasso.gtd" "~/Planning/concepts/blog.gtd" "~/Planning/innovita/casino.gtd" "~/Planning/innovita/gamebattle.gtd" "~/Planning/innovita/dreamflyer.gtd" "~/Planning/educoncepts.gtd" "~/Planning/innovita.gtd" "~/Planning/maybe.gtd" "~/Planning/journal.gtd" "~/Planning/gtd.gtd")))
 '(org-agenda-include-diary t)
 '(org-agenda-ndays 14)
 '(org-agenda-window-setup (quote current-window))
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-link-abbrev-alist (quote (("as24trac" . "https://iss-trac.autoscout24.com/projects/ISS/ticket/"))))
 '(org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame) (gnus . gnus) (file . find-file-other-window))))
 '(org-tags-column 80)
 '(quack-fontify-style nil)
 '(quack-programs (quote ("gsi " "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~/syntax-case.scm -" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M
    errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(rails-ws:default-server-type "thin")
 '(recentf-exclude (quote ("/tmp/" "/ssh:")))
 '(recentf-mode t)
 '(ruby-electric-expand-delimiters-list nil)
 '(ruby-insert-encoding-magic-comment nil)
 '(ruby-use-encoding-map nil)
 '(safe-local-variable-values (quote ((org-export-publishing-directory . "tmp") (encoding . iso-8859-15) (Package . LISP-UNIT))))
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-trailing-whitespace nil)
 '(size-indication-mode t)
 '(slime-autodoc-mode t t)
 '(smart-tab-using-hippie-expand t)
 '(smex-prompt-string "M-x ")
 '(speedbar-default-position (quote right))
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.svn\\|_darcs\\)\\'")
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images t)
 '(svn-status-hide-unmodified t)
 '(tooltip-delay 1.5)
 '(tooltip-mode t)
 '(transient-mark-mode (quote (only . t)))
 '(truncate-lines nil)
 '(truncate-partial-width-windows nil)
 '(twit-user-image-dir "~/tmp/twitter-images/")
 '(viper-emacs-state-mode-list (quote (custom-mode dired-mode efs-mode tar-mode browse-kill-ring-mode recentf-mode recentf-dialog-mode occur-mode mh-folder-mode gnus-group-mode gnus-article-mode gnus-summary-mode completion-list-mode help-mode Info-mode Buffer-menu-mode compilation-mode rcirc-mode jde-javadoc-checker-report-mode view-mode vm-mode vm-summary-mode etags-select-mode)))
 '(viper-vi-state-mode-list (quote (fundamental-mode makefile-mode awk-mode m4-mode xrdb-mode winmgr-mode autoconf-mode cvs-edit-mode html-mode html-helper-mode emacs-lisp-mode lisp-mode lisp-interaction-mode jde-mode java-mode cc-mode c-mode c++-mode objc-mode fortran-mode f90-mode basic-mode bat-mode asm-mode prolog-mode flora-mode sql-mode text-mode indented-text-mode tex-mode latex-mode bibtex-mode ps-mode diff-mode idl-mode perl-mode cperl-mode javascript-mode tcl-mode python-mode sh-mode ksh-mode csh-mode mh-show-mode clojure-mode)))
 '(viper-want-ctl-h-help t)
 '(visible-bell t)
 '(whitespace-check-indent-whitespace nil)
 '(whitespace-display-in-modeline nil)
 '(whitespace-display-spaces-in-color nil)
 '(whitespace-errbuf "*Messages*")
 '(whitespace-global-mode nil)
 '(whitespace-silent t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#121212" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :foundry "bitstream" :family "Bitstream Vera Sans Mono"))))
 '(darcsum-need-action-marked-face ((t (:foreground "forestgreen"))))
 '(diredp-dir-heading ((t (:background "black" :foreground "Blue" :weight bold))))
 '(diredp-dir-priv ((t (:foreground "DarkRed" :underline t))))
 '(diredp-exec-priv ((t nil)))
 '(diredp-flag-mark-line ((t (:box (:line-width 2 :color "blue" :style released-button)))))
 '(diredp-no-priv ((t (:background "black"))))
 '(diredp-read-priv ((t nil)))
 '(diredp-write-priv ((t nil)))
 '(flymake-errline ((((class color)) (:background "red3"))))
 '(flymake-warnline ((((class color)) (:background "red4"))))
 '(highlight ((t (:background "white" :foreground "black"))))
 '(highline-face ((t (:background "floral white"))))
 '(hl-line ((t (:background "black"))))
 '(ido-first-match ((t (:background "black" :foreground "white"))))
 '(ido-indicator ((((class color)) (:background "black" :foreground "yellow" :width condensed))))
 '(ido-only-match ((((class color)) (:background "black" :foreground "ForestGreen"))))
 '(ido-subdir ((((class color)) (:background "black" :foreground "red"))))
 '(mmm-code-submode-face ((((class color) (background dark)) (:background "DarkSlateGrey")) (((class color) (background light)) (:background "Thistle2"))))
 '(mmm-comment-submode-face ((((class color) (background dark)) (:background "DarkSlateGrey" :foreground "OrangeRed")) (((class color) (background light)) (:background "khaki3"))))
 '(mmm-default-submode-face ((t (:background "DarkBlue"))))
 '(mmm-output-submode-face ((((class color) (background dark)) (:background "Navy")) (((class color) (background light)) (:background "Thistle3"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "DarkOliveGreen"))))
 '(org-agenda-date-today ((t (:background "CadetBlue" :foreground "Black" :weight extra-bold))) t)
 '(org-column ((t (:background "grey30" :strike-through nil :underline nil :slant normal :weight normal :height 1.0 :family "DejaVu Sans Mono"))))
 '(org-column-title ((t (:inherit org-column :background "grey30" :underline t :weight bold))))
 '(org-tag ((t (:underline t))))
 '(twit-logo-face ((((class color)) (:family "mono" :weight bold :height 1.5 :box (:line-width 1 :color "PowderBlue" :style 0) :background "Yellow4" :foreground "Yellow1" :underline "DeepSkyBlue")) (t (:inverse nil))))
 '(twit-title-face ((((background light)) (:background "PowderBlue" :underline "DeepSkyBlue" :box (:line-width 2 :color "PowderBlue" :style 0))) (((background dark)) (:background "Black" :underline "Grey" :box (:line-width 2 :color "DarkGreen" :style 0))) (t (:underline "white")))))
