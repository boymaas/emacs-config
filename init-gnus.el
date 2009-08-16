(setq user-full-name "KXCG Maas"
      user-mail-address "boy.maas@gmail.com"
      gnus-local-organization "Innovita.nl"
      gnus-select-method '(nntp "news.opera.com")
      message-yank-prefix "| ")

(load-library "smtpmail")
(load-library "nnimap")
(load-library "starttls")
;;(require 'nnir)
 
(setq gnus-select-method '(nnimap "imap.gmail.com"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-authinfo-file "~/.authinfo")
                                  (nnir-search-engine imap)
                                  (nnimap-stream ssl)
                                  ))
 

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

(setq starttls-use-gnutls nil)
(mailcap-add "image/jpeg" "display")

(add-hook 'gnus-topic-mode-hook 'gnus-topic-mode)
 
(setq gnus-outgoing-message-group "[Google Mail]/Sent Mail")
(setq gnus-extract-address-components
      'mail-extract-address-components)
 
(require 'bbdb)
(bbdb-initialize)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-message)
 
;;(require 'moy-bbdb)
;;(autoload 'bbdb/send-hook "moy-bbdb" 
;;   "Function to be added to `message-send-hook' to notice records when sending messages" t)
;;(add-hook 'message-send-hook 'bbdb/send-hook) ; If you use Gnus
 
;; (setq gnus-invalid-group-regexp "[:`'\"]\\|^$")
(setq gnus-ignored-newsgroups "")
(setq gnus-outgoing-message-group "[Google Mail]/Sent Mail")
 
(setq gnus-summary-mark-below 0)

;; when browsing an article i would like to browse
;; a link into firefox using M
(define-key gnus-article-mode-map "M" 'w3m-view-url-with-external-browser)

;; |-----------------------------------------------------+----------------------------------------+-------------------------------------------|
;; | Action on client                                    | Result in Gmail on the web             | Command in Gnus                           |
;; |-----------------------------------------------------+----------------------------------------+-------------------------------------------|
;; | Open a message                                      | Mark a message as read                 | RET                                       |
;; | Flag a message                                      | Apply a star to the message            | !                                         |
;; | Unflag a message                                    | Remove the star from the message       | M-u                                       |
;; | Move a message to a folder                          | Apply a label to the message           | B m                                       |
;; | Move a message to a folder within a folder          | Apply a label showing folder hierarchy | B m                                       |
;; | Create a folder                                     | Create a label                         | B m to nonexistent folder will create it. |
;; | Move a message to [Gmail]/Spam                      | Report a message as spam               | B m [Gmail]Spam RET                       |
;; | Move a message to [Gmail]/Trash                     | Move a message to Trash                | B m [Gmail]Trash RET                      |
;; | Send a message                                      | Store message in Sent Mail             | m                                         |
;; | Delete a message in inbox                           | Remove the message from inbox          | B DEL                                     |
;; | Delete a message from a folder                      | Remove that label from the message     | B DEL                                     |
;; | Delete a message from [Gmail]/Spam or [Gmail]/Trash | Delete the message permanently         | B DEL                                     |
;; |-----------------------------------------------------+----------------------------------------+-------------------------------------------|

(provide 'init-gnus)
