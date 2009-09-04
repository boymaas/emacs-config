(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

(setq viper-shift-width 4) ; don't touch or else...

(viper-record-kbd-macro ";t" 'vi-state [(control x) b m o d e l \  t e s t \ (control c) (control \,) (control x) b \] 'clojure-mode)

(viper-record-kbd-macro ";c" 'vi-state [(control c) (control k)] 'clojure-mode)

