;; This file contains stub definitions from ivy.el which allow
;; ivy-prescient.el to be byte-compiled in the absence of ivy.el.

(defvar ivy--directory nil)
(defvar ivy--subexps nil)
(defvar ivy-initial-inputs-alist nil)
(defvar ivy-last nil)
(defvar ivy-marked-candidates nil)
(defvar ivy-re-builders-alist nil)
(defvar ivy-sort-matches-functions-alist nil)
(defvar ivy-sort-functions-alist nil)

(defun ivy--directory-enter ())
(defun ivy--get-action (collection))
(defun ivy--sort-function (collection))
(defun ivy-completion-in-region (start end collection &optional predicate))
(defun ivy-read (prompt collection &rest args))
(defun ivy-state-collection (struct))
(defun ivy-state-sort (struct))
(defun ivy-string< (x y))

(provide 'ivy)
