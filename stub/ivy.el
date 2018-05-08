;; This file contains stub definitions from ivy.el which allow
;; ivy-prescient.el to be byte-compiled in the absence of ivy.el.

(defvar ivy-re-builders-alist nil)
(defvar ivy-sort-functions-alist nil)

(defun ivy--sort-function (collection))
(defun ivy--sort-maybe (collection))
(defun ivy-read (prompt collection &rest args))

(provide 'ivy)
