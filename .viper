(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

(setq viper-shift-width 4) ; don't touch or else...


(viper-record-kbd-macro ";c" 'vi-state [(control x) (control s) (control c) (control k)] 'clojure-mode)

(viper-record-kbd-macro ";tt" 'vi-state [\; \; m o d e l t e s t \ (control x) (control s) (control c) (control \,) \; \; \] 'clojure-mode)

(viper-record-kbd-macro "z" 'vi-state [\; \; (control m)] 't)

(viper-record-kbd-macro ";r" 'vi-state [\; \o r e p l \ ] 'clojure-mode)

(viper-record-kbd-macro "gv" 'vi-state [(control x) (control x)] 't)

(viper-record-kbd-macro ";t" 'vi-state [(control c) (control \,)] 'clojure-mode)
