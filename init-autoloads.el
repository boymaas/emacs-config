;;----------------------------------------------------------------------------
;; Crontab mode
;;----------------------------------------------------------------------------
(autoload 'crontab-mode "crontab-mode" "Mode for editing crontab files" t)
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")

;;----------------------------------------------------------------------------
;; Textile-mode
;;----------------------------------------------------------------------------
(autoload 'textile-mode "textile-mode" "Mode for editing Textile documents" t)

;;----------------------------------------------------------------------------
;; Markdown-mode
;;----------------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode" "Mode for editing Markdown documents" t)

;;----------------------------------------------------------------------------
;; Regex-tool
;;----------------------------------------------------------------------------
(autoload 'regex-tool "regex-tool" "Mode for exploring regular expressions" t)
(setq regex-tool-backend 'perl)

;;----------------------------------------------------------------------------
;; Gnuplot
;;----------------------------------------------------------------------------
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)

;;----------------------------------------------------------------------------
;; Luke Gorrie's "lively.el"
;;----------------------------------------------------------------------------
(autoload 'lively "lively" "Interactively updating text" t)

(provide 'init-autoloads)