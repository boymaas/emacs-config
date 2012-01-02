;;----------------------------------------------------------------------------
;; Extensions -> Modes
;;----------------------------------------------------------------------------
(add-auto-mode 'html-mode "\\.(jsp|tmpl)$")
(add-auto-mode 'tcl-mode "Portfile$")
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")
(add-auto-mode 'yaml-mode "\\.ya?ml$")
(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")

(provide 'init-auto-mode-settings)