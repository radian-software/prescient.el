;; This file contains stub definitions from ivy.el which allow
;; ivy-prescient.el to be byte-compiled in the absence of ivy.el.

(defvar ivy--subexps nil)
(defvar ivy-initial-inputs-alist nil)
(defvar ivy-re-builders-alist nil)
(defvar ivy-sort-functions-alist nil)
(defvar ivy-marked-candidates nil)
(defvar ivy-last nil)
(defvar ivy--directory nil)

(defun ivy-read (prompt collection &rest args))
(defun ivy-state-sort (struct))
(defun ivy-state-collection (struct))
(defun ivy--sort-function (collection))
(defun ivy--get-action (collection))
(defun ivy--directory-enter ())

(provide 'ivy)
