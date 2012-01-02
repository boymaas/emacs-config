;;----------------------------------------------------------------------------
;; Defsubmodule macro
;; Function does the following things:
;; 1. defines a variable based on name: name -> *<name>-support-enabled* which
;;    can be set above
;; 2. defines a function based on name: name -> bmaas-enable-<name>
;;    and places the initialization code in there
;; 3. Runs the initialization mode base on the previously set value as defined
;;    at point 1
;;----------------------------------------------------------------------------
(defmacro defsubmodule (name &rest body)
  (let ((name (concatenate 'string "*" (symbol-name name) "-support-enabled*"))
	(function-name (concatenate 'string "module-enable-" (symbol-name name))))
    `(progn
       ;; define function
       (defun ,(intern function-name) ()
	 (interactive)
	 ,@body)
       ;; make sure variable is defined
       (unless (boundp (quote ,(intern name))) 
	       (setq ,(intern name) nil))
       ;; if is set to true before .. run initialisation function
       (when ,(intern name)
	 (,(intern function-name))))))

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(require 'find-func)
(defun directory-of-library (library-name)
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


;;----------------------------------------------------------------------------
;; Easy way to check that we're operating on a specific file type
;;----------------------------------------------------------------------------
(defun filename-has-extension-p (extensions)
  (and buffer-file-name
       (string-match (concat "\\." (regexp-opt extensions t) "\\($\\|\\.\\)") buffer-file-name)))


;;----------------------------------------------------------------------------
;; Locate executables
;;----------------------------------------------------------------------------
(defun find-executable (name)
  "Return the full path of an executable file name `name'
in `exec-path', or nil if no such command exists"
  (loop for dir in exec-path
	for full-path = (expand-file-name (concat dir "/" name))
	when (file-executable-p full-path)
	return full-path))

;;----------------------------------------------------------------------------
;; Easily count words (http://emacs-fu.blogspot.com/2009/01/counting-words.html)
;;----------------------------------------------------------------------------
(defun count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  (interactive)
  (or (buffer-file-name) (error "no file is currently being edited"))
  (when (yes-or-no-p "Really delete this file?")
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))

(provide 'init-utility-functions)