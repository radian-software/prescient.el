;;; ivy-prescient.el --- prescient.el + Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2018 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 7 Aug 2017
;; Package-Requires: ((emacs "25.1") (prescient "1.0") (ivy "0.10.0"))
;; Version: 1.0

;;; Commentary:

;; ivy-prescient.el provides an interface for using prescient.el to
;; sort and filter candidates in Ivy menus. To enable its
;; functionality, turn on `ivy-prescient-mode' in your init-file or
;; interactively.

;; For more information, see https://github.com/raxod502/prescient.el.

;;; Code:

;;;; Libraries

(eval-when-compile
  (require 'cl-macs)
  (require 'subr-x))

(require 'ivy)
(require 'prescient)

;;;; User options

(defcustom ivy-prescient-excluded-commands '(swiper)
  "Commands for which candidates should not be remembered."
  :group 'prescient
  :type '(list symbol))

;;;; Minor mode

(defun ivy-prescient-re-builder (query)
  "Generate an Ivy-formatted regexp list for the given QUERY string.
This is for use in `ivy-re-builders-alist'."
  (setq ivy--subexps 0)
  (or
   (mapcar
    (lambda (regexp)
      (setq ivy--subexps (max ivy--subexps (regexp-opt-depth regexp)))
      (cons regexp t))
    (prescient-filter-regexps query 'with-groups))
   ;; For some reason, Ivy doesn't seem to like to be given an empty
   ;; list of regexps. Instead, it wants an empty string.
   ""))

(defvar ivy-prescient--old-re-builder nil
  "Previous default value in `ivy-re-builders-alist'.")

(defalias 'ivy-prescient-sort-function #'prescient-sort-compare
  "Comparison function that uses prescient.el to sort candidates.
This is for use in `ivy-sort-functions-alist'.")

(defvar ivy-prescient--old-ivy-sort-function nil
  "Previous default value in `ivy-sort-functions-alist'.")

(defalias 'ivy-prescient-sort-file-function #'prescient-sort-compare
  "Comparison function that uses prescient.el to sort files.
This is for use in `ivy-sort-functions-alist'.")

(defvar ivy-prescient--old-ivy-sort-file-function nil
  "Previous value for sorting files in `ivy-sort-functions-alist'.
This is the value that was associated to
`read-file-name-internal'.")

(cl-defun ivy-prescient-read
    (ivy-read prompt collection &rest rest &key action caller
              &allow-other-keys)
  "Delegate to `ivy-read', recording information for `prescient-remember'.
This is an `:around' advice for `ivy-read'."
  (apply ivy-read prompt collection
         (append `(:action
                   ,(lambda (result)
                      (unless (memq caller ivy-prescient-excluded-commands)
                        (prescient-remember result))
                      (when action
                        (funcall action result))))
                 rest)))

;;;###autoload
(define-minor-mode ivy-prescient-mode
  "Minor mode to use prescient.el in Ivy menus."
  :global t
  (if ivy-prescient-mode
      (progn
        (setq ivy-prescient--old-re-builder
              (alist-get t ivy-re-builders-alist))
        (setf (alist-get t ivy-re-builders-alist)
              #'ivy-prescient-re-builder)
        (setq ivy-prescient--old-ivy-sort-function
              (alist-get t ivy-sort-functions-alist))
        (setf (alist-get t ivy-sort-functions-alist)
              #'ivy-prescient-sort-function)
        (setq ivy-prescient--old-ivy-sort-file-function
              (alist-get #'read-file-name-internal ivy-sort-functions-alist))
        (setf (alist-get #'read-file-name-internal ivy-sort-functions-alist)
              #'ivy-prescient-sort-file-function)
        (advice-add #'ivy-read :around #'ivy-prescient-read))
    (when (equal (alist-get t ivy-re-builders-alist)
                 #'ivy-prescient-re-builder)
      (setf (alist-get t ivy-re-builders-alist)
            ivy-prescient--old-re-builder))
    (when (equal (alist-get t ivy-sort-functions-alist)
                 #'ivy-prescient-sort-function)
      (setf (alist-get t ivy-sort-functions-alist)
            ivy-prescient--old-ivy-sort-function))
    (when (equal (alist-get #'read-file-name-internal
                            ivy-sort-functions-alist)
                 #'ivy-prescient-sort-file-function)
      (setf (alist-get #'read-file-name-internal ivy-sort-functions-alist)
            ivy-prescient--old-ivy-sort-file-function))
    (advice-remove #'ivy-read #'ivy-prescient-read)))

;;;; Closing remarks

(provide 'ivy-prescient)

;;; ivy-prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
