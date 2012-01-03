;;----------------------------------------------------------------------------
;; Extensions -> Modes
;;----------------------------------------------------------------------------
(add-auto-mode 'html-mode "\\.(jsp|tmpl)$")
(add-auto-mode 'tcl-mode "Portfile$")
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")
(add-auto-mode 'yaml-mode "\\.ya?ml$")
(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")
(add-auto-mode 'markdown-mode "\\.mkd")

(provide 'init-auto-mode-settings)