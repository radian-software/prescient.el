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

(defcustom ivy-prescient-excluded-commands
  '(counsel-ag
    counsel-expression-history
    counsel-git-grep
    counsel-grep
    counsel-mark-ring
    counsel-minibuffer-history
    counsel-shell-command-history
    counsel-yank-pop
    swiper)
  "Commands for which candidates should not be remembered."
  :group 'prescient
  :type '(list symbol))

(defcustom ivy-prescient-sort-commands
  '(counsel-find-file
    counsel-find-library)
  "Commands for which candidates should always be sorted.
This allows you to enable sorting for commands which call
`ivy-read' with a nil value for `:sort'."
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

(defun ivy-prescient--wrap-action (caller action)
  "Wrap an action for use in `ivy-read'.
CALLER is the `:caller' argument to `ivy-read', and ACTION is the
original action, a function. Return a new function that also
invokes `prescient-remember'."
  (lambda (result)
    (unless (memq caller ivy-prescient-excluded-commands)
      (prescient-remember result))
    (when action
      (funcall action result))))

(cl-defun ivy-prescient-read
    (ivy-read prompt collection &rest rest &key action caller
              &allow-other-keys)
  "Delegate to `ivy-read', handling persistence and sort customization.
If the `:caller' passed to `ivy-read' is a member of
`ivy-prescient-sort-commands', then `:sort' is unconditionally
enabled. Also, `:action' is modified so that the selected
candidate is passed to `prescient-remember'.

This is an `:around' advice for `ivy-read'. IVY-READ is the
original definition of `ivy-read', and PROMPT, COLLECTION are the
same as in `ivy-read'. REST is the list of keyword arguments, and
keyword arguments ACTION, CALLER are the same as in `ivy-read'."
  (apply ivy-read prompt collection
         (append `(:action
                   ,(if (or (null action) (functionp action))
                        (ivy-prescient--wrap-action caller action)
                      (mapcar
                       (lambda (entry)
                         (if (listp entry)
                             (cl-destructuring-bind (key fun . rest) entry
                               (apply #'list key
                                      (ivy-prescient--wrap-action caller fun)
                                      rest))
                           entry))
                       action))
                   ,@(when (memq caller ivy-prescient-sort-commands)
                       `(:sort t)))
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
