(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)


(viper-record-kbd-macro ";c" 'vi-state [(control x) (control s) (control c) (control k)] 'clojure-mode)

(viper-record-kbd-macro ";t" 'vi-state [(control x) (control s) (control c) (control \,)]  'clojure-mode)
(viper-record-kbd-macro ";tt" 'vi-state [\; \; m o d e l t e s t \ (control x) (control s) (control c) (control \,) \; \; \] 'clojure-mode)

(viper-record-kbd-macro "z" 'vi-state [\; \; (control m)] 't)

(viper-record-kbd-macro ";r" 'vi-state [\; \o r e p l \ ] 'clojure-mode)

(viper-record-kbd-macro "gv" 'vi-state [(control x) (control x)] 't)


(viper-record-kbd-macro ";[" 'vi-state [\; x c l i p b o a r d - k i l l - r i g n (control h) (control h) n g - s a v e (control m)] 't)

(setq viper-shift-width 2) ; don't touch or else...
