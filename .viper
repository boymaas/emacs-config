(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

(setq viper-shift-width 4) ; don't touch or else...


(viper-record-kbd-macro ";c" 'vi-state [(control x) (control s) (control c) (control k)] 'clojure-mode)

(viper-record-kbd-macro ";t" 'vi-state [\; \; m o d e l t e s t \ (control x) (control s) (control c) (control \,) \; \; \] 'clojure-mode)
