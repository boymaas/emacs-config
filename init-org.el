(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/contrib/lisp" load-path))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.gtd$" . org-mode))

(setq org-completion-use-ido t)


; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc 
(setq org-outline-path-complete-in-steps t)


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))


;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; org-checklist functionality
(eval-after-load "org"
  '(require 'org-checklist))

(add-hook 'org-mode-hook
          (function (lambda ()
                  (local-set-key (kbd "C-c b") 'org-toggle-checkbox) )))

(require 'remember)

;; (org-remember-insinuate) ;; see below
;; insinuate didn't word zo there 3 rows are a quick workaround
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; the remember templates

;;     %^{prompt}  prompt the user for a string and replace this sequence with it.
;;                 You may specify a default value and a completion table with
;;                 %^{prompt|default|completion2|completion3...}
;;                 The arrow keys access a prompt-specific history.
;;     %a          annotation, normally the link created with org-store-link
;;     %A          like %a, but prompt for the description part
;;     %i          initial content, the region when remember is called with C-u.
;;                 The entire text will be indented like %i itself.
;;     %t          timestamp, date only
;;     %T          timestamp with date and time
;;     %u, %U      like the above, but inactive timestamps
;;     %^t         like %t, but prompt for date.  Similarly %^T, %^u, %^U
;;                 You may define a prompt like %^{Birthday}t
;;     %n          user name (taken from user-full-name)
;;     %c          Current kill ring head.
;;     %x          Content of the X clipboard.
;;     %^C         Interactive selection of which kill or clip to use.
;;     %^L         Like %^C, but insert as link.
;;     %^g         prompt for tags, with completion on tags in target file.
;;     %k          title of currently clocked task
;;     %K          link to currently clocked task
;;     %^G         prompt for tags, with completion all tags in all agenda files.
;;     %^{prop}p   Prompt the user for a value for property prop
;;     %:keyword   specific information for certain link types, see below
;;     % [file]     insert the contents of the file given by file
;;     % (sexp)     evaluate Elisp sexp and replace with the result
;;     %!          immediately store note after completing the template
;;                 (skipping the C-c C-c that normally triggers storing)
;;     %&          jump to target location immediately after storing note

(setq org-remember-templates
     '(
      ("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" (org-path "gtd.gtd") "Tasks")
      ("Private" ?p "\n* %^{topic} %T \n%i%?\n" (org-path "journal.gtd"))
      ))

;; global shortcut to invoke remember
(define-key global-map "\C-cr" 'org-remember)

;; Standard org directory and a directory to
;; append the path after.
(setq org-directory "~/Planning/")
(defun org-path (fname)
  (concat org-directory fname))

;; some handy settings
(setq org-timeline-show-empty-dates t)
(setq org-insert-mode-line-in-empty-file t)

;; Where to put the refile trees
(setq org-refile-targets (quote (("~/Planning/gtd.gtd" :maxlevel . 1) ("~/Planning/notes.gtd" :level . 2))))
;;(setq org-refile-targets (quote ((nil :maxlevel . 2) (org-agenda-files :maxlevel . 2))))

;; some fast access to the agendas custom commands
(setq org-agenda-custom-commands
      '(
        ("P" "Projects"   
         ((tags "PROJECT")))
        ("H" "Office and Home Lists"
         ((agenda)
          (tags-todo "OFFICE")
          (tags-todo "HOME")
          (tags-todo "COMPUTER")
          (tags-todo "DVD")
          (tags-todo "READING")))

        ("D" "Daily Action List"
         (
          (agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0) ))))))


;; quick access function to our todo file
(defun gtd ()
    (interactive)
    (find-file (org-path "gtd.gtd")))
;; and a global shortcut for it
(global-set-key (kbd "C-c g") 'gtd)

(provide 'init-org)
