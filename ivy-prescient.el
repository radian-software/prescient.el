;;; ivy-prescient.el --- prescient.el + Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2018 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 7 Aug 2017
;; Package-Requires: ((emacs "25.1") (prescient "2.2.2") (ivy "0.11.0"))
;; Version: 2.2.2

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

(require 'map)

(require 'ivy)
(require 'prescient)

;;;; User options

(defcustom ivy-prescient-sort-commands '(counsel-find-library)
  "Commands for which candidates should always be sorted.
This allows you to enable sorting for commands which call
`ivy-read' with a nil value for `:sort'."
  :group 'prescient
  :type '(list symbol))

(defcustom ivy-prescient-retain-classic-highlighting nil
  "Whether to emulate the way Ivy highlights candidates as closely as possible.
With the default value, nil, the entire match is highlighted with
`ivy-minibuffer-match-face-1' while initials in an initialism are
highlighted with `ivy-minibuffer-match-face-2' through
`ivy-minibuffer-match-face-4'. With a non-nil value, substring
matches are also highlighted using `ivy-minibuffer-match-face-2'
through `ivy-minibuffer-match-face-4', meaning that the only use
of `ivy-minibuffer-match-face-1' is in between the initials of an
initialism.

Note that a non-nil value for this variable emulates the
highlighting behavior of `ivy--regex-ignore-order', not the
default `ivy--regex-plus', since `ivy-prescient' allows
out-of-order matching."
  :group 'prescient
  :type 'boolean)

(defcustom ivy-prescient-enable-filtering t
  "Whether to enable filtering by `ivy-prescient'.
If nil, then `ivy-prescient-mode' does not change the filtering
behavior of Ivy from the default. See Ivy documentation for how to
configure filtering yourself. Changing this variable will not
take effect until `ivy-prescient-mode' has been reloaded."
  :group 'prescient
  :type 'boolean)

(defcustom ivy-prescient-enable-sorting t
  "Whether to enable sorting by `ivy-prescient'.
If nil, then `ivy-prescient-mode' does not change the sorting
behavior of Ivy from the default. See Ivy documentation for how
to configure sorting yourself. Changing this variable will not
take effect until `ivy-prescient-mode' has been reloaded."
  :group 'prescient
  :type 'boolean)

;;;; Minor mode

(defvar ivy-prescient--last-valid-regexp-list ""
  "Last return value of `ivy-prescient-re-builder'.
This is used to ensure that when the user provides an invalid
regexp, we can instead return the last valid regexp they entered.
This is important since Ivy crashes when given an invalid
regexp.")

(defun ivy-prescient-re-builder (query)
  "Generate an Ivy-formatted regexp list for the given QUERY string.
This is for use in `ivy-re-builders-alist'."
  (cl-block nil
    (let ((orig-ivy-subexps ivy--subexps))
      (setq ivy--subexps 0)
      (save-match-data
        (setq
         ivy-prescient--last-valid-regexp-list
         (or
          (mapcar
           (lambda (regexp)
             (condition-case _
                 (string-match regexp "")
               (invalid-regexp
                (setq ivy--subexps orig-ivy-subexps)
                (cl-return ivy-prescient--last-valid-regexp-list)))
             (setq ivy--subexps (max ivy--subexps (regexp-opt-depth regexp)))
             (cons regexp t))
           (prescient-filter-regexps
            query
            (if ivy-prescient-retain-classic-highlighting
                'all
              'with-groups)))
          ;; For some reason, Ivy doesn't seem to like to be given an empty
          ;; list of regexps. Instead, it wants an empty string.
          ""))))))

(defvar ivy-prescient--old-re-builder nil
  "Previous default value in `ivy-re-builders-alist'.")

(defun ivy-prescient-sort-function (c1 c2)
  "Comparison function that uses prescient.el to sort candidates.
This is for use in `ivy-sort-functions-alist'. C1 and C2 are
arbitrary candidates to be compared; they need not be strings."
  ;; For some reason, Ivy supports candidates that are lists, and just
  ;; takes their cars. I guess we have to support that too.
  (when (listp c1)
    (setq c1 (car c1)))
  (when (listp c2)
    (setq c2 (car c2)))
  (prescient-sort-compare c1 c2))

(defvar ivy-prescient--old-ivy-sort-function nil
  "Previous default value in `ivy-sort-functions-alist'.")

(defalias 'ivy-prescient-sort-file-function #'prescient-sort-compare
  "Comparison function that uses prescient.el to sort files.
This is for use in `ivy-sort-functions-alist'.")

(defvar ivy-prescient--old-ivy-sort-file-function nil
  "Previous value for sorting files in `ivy-sort-functions-alist'.
This is the value that was associated to
`read-file-name-internal'.")

(defvar ivy-prescient--old-initial-inputs-alist nil
  "Previous value of `ivy-initial-inputs-alist'.")

(defun ivy-prescient--wrap-action (action)
  "Wrap an action for use in `ivy-read'.
ACTION is the original action, a function. Return a new function
that also invokes `prescient-remember'."
  (if (or ivy-marked-candidates
          (not (memq (let ((sort (ivy-state-sort ivy-last))
                           (coll (ivy-state-collection ivy-last)))
                       (cond ((functionp sort)
                              sort)
                             ((or sort (eq coll #'read-file-name-internal))
                              (ivy--sort-function coll))))
                     '(ivy-prescient-sort-file-function
                       ivy-prescient-sort-function))))
      action
    (let ((dir ivy--directory))
      (lambda (x)
        (let ((cand x))
          (when (listp cand) (setq cand (car x)))
          (when dir (setq cand (string-remove-prefix dir cand)))
          (prescient-remember cand))
        (funcall action x)))))

(defun ivy-prescient-enable-extra-sort (args)
  "Enable sorting of `ivy-prescient-sort-commands'.
 If the `:caller' passed to `ivy-read' is a member of
`ivy-prescient-sort-commands', then `:sort' is unconditionally
enabled."
  (append args (and (memq (plist-get args :caller)
                          ivy-prescient-sort-commands)
                    '(:sort t))))

;;;###autoload
(define-minor-mode ivy-prescient-mode
  "Minor mode to use prescient.el in Ivy menus."
  :global t
  :group 'prescient
  (if ivy-prescient-mode
      (progn
        (when ivy-prescient-enable-filtering
          (setq ivy-prescient--old-re-builder
                (alist-get t ivy-re-builders-alist))
          (setf (alist-get t ivy-re-builders-alist)
                #'ivy-prescient-re-builder)
          (setq ivy-prescient--old-initial-inputs-alist
                ivy-initial-inputs-alist)
          (setq ivy-initial-inputs-alist nil))
        (when ivy-prescient-enable-sorting
          (setq ivy-prescient--old-ivy-sort-function
                (alist-get t ivy-sort-functions-alist))
          (setf (alist-get t ivy-sort-functions-alist)
                #'ivy-prescient-sort-function)
          (setq ivy-prescient--old-ivy-sort-file-function
                (alist-get #'read-file-name-internal ivy-sort-functions-alist))
          (setf (alist-get #'read-file-name-internal ivy-sort-functions-alist)
                #'ivy-prescient-sort-file-function)
          (advice-add #'ivy-read :filter-args #'ivy-prescient-enable-extra-sort)
          (advice-add #'ivy--get-action :filter-return #'ivy-prescient--wrap-action)))
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
    (unless ivy-initial-inputs-alist
      (dolist (pair (reverse ivy-prescient--old-initial-inputs-alist))
        (setf (alist-get (car pair) ivy-initial-inputs-alist) (cdr pair))))
    (advice-remove #'ivy-read #'ivy-prescient-enable-extra-sort)
    (advice-remove #'ivy--get-action #'ivy-prescient--wrap-action)))

;;;; Closing remarks

(provide 'ivy-prescient)

;;; ivy-prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; checkdoc-verb-check-experimental-flag: nil
;; End:
