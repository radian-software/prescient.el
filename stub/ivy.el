;; This file contains stub definitions from ivy.el which allow
;; ivy-prescient.el to be byte-compiled in the absence of ivy.el.

(defvar ivy--subexps nil)
(defvar ivy-initial-inputs-alist nil)
(defvar ivy-re-builders-alist nil)
(defvar ivy-sort-functions-alist nil)

(defun ivy-read (prompt collection &rest args))

(provide 'ivy)
