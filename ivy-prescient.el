;;; ivy-prescient.el --- Prescient sorting for Ivy. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 7 Aug 2017
;; Package-Requires: ((emacs "25.1") (prescient "1.0") (ivy "0.10.0"))
;; Version: 1.0

;;; Commentary:

;; This package is under construction.

;;; Code:

;;;; Libraries

(eval-when-compile
  (require 'cl-macs))

(require 'ivy)
(require 'prescient)

;;;; Minor mode

(defalias 'ivy-prescient-filter #'prescient-filter
  "Use prescient.el for Ivy filtering.
This is an `:override' advice for `ivy--filter' that is activated
when `ivy-prescient-mode' is enabled.")

(defun ivy-prescient-sort-function (collection)
  "Retrieve sort function for COLLECTION from `ivy-sort-functions-alist'.
This is an `:override' advice for `ivy--sort-function' which
fixes what appears to be a bug whereby the default sort function
is not respected."
  (alist-get collection ivy-sort-functions-alist
             (alist-get t ivy-sort-functions-alist)))

(defvar ivy-prescient--old-ivy-sort-function nil
  "Previous default value in `ivy-sort-functions-alist'.")

(cl-defun ivy-prescient-read (ivy-read prompt collection &rest rest &key action caller
                                       &allow-other-keys)
  "Delegate to `ivy-read', recording information for `prescient-remember'.
This is an `:around' advice for `ivy-read'."
  (apply ivy-read prompt collection
         (append `(:action ,(lambda (result)
                              (prescient-remember result)
                              (when action
                                (funcall action result))))
                 rest)))

;;;###autoload
(define-minor-mode ivy-prescient-mode
  "Minor mode to use prescient.el in Ivy menus."
  :global t
  (if ivy-prescient-mode
      (progn
        (advice-add #'ivy--filter :override #'ivy-prescient-filter)
        (setq ivy-prescient--old-ivy-sort-function
              (alist-get t ivy-sort-functions-alist))
        (setf (alist-get t ivy-sort-functions-alist)
              #'prescient-sort-compare)
        (advice-add #'ivy--sort-function :override #'ivy-prescient-sort-function)
        (advice-add #'ivy-read :around #'ivy-prescient-read))
    (advice-remove #'ivy--filter #'ivy-prescient-filter)
    (when (equal (alist-get t ivy-sort-functions-alist)
                 #'prescient-sort-compare)
      (setf (alist-get t ivy-sort-functions-alist)
            ivy-prescient--old-ivy-sort-function))
    (advice-remove #'ivy--sort-function #'ivy-prescient-sort-function)
    (advice-remove #'ivy-read #'ivy-prescient-read)))

;;;; Closing remarks

(provide 'ivy-prescient)

;;; ivy-prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
