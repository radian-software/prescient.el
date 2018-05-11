;; This file contains stub definitions from ivy.el which allow
;; ivy-prescient.el to be byte-compiled in the absence of ivy.el.

(defvar ivy-highlight-functions-alist nil)
(defvar ivy-minibuffer-faces nil)
(defvar ivy-re-builders-alist nil)
(defvar ivy-sort-functions-alist nil)
(defvar ivy-text nil)

(defun ivy--sort-function (collection))
(defun ivy--sort-maybe (collection))
(defun ivy-add-face-text-property (start end face str))
(defun ivy-read (prompt collection &rest args))

(provide 'ivy)
