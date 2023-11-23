;;; ivy-prescient.el --- prescient.el + Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Radian LLC and contributors

;; Author: Radian LLC <contact+prescient@radian.codes>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 1 May 2018
;; Package-Requires: ((emacs "25.1") (prescient "6.1.0") (ivy "0.11.0"))
;; SPDX-License-Identifier: MIT
;; Version: 6.2.0

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

(defcustom ivy-prescient-sort-commands
  '(:not swiper swiper-isearch ivy-switch-buffer)
  "Control which commands have their candidates sorted by `ivy-prescient'.
If nil, then sorting is disabled for all commands. If t, then
sorting is enabled for all commands. If a list of commands, then
only those commands have their candidates sorted. If a list
starting with the symbol `:not', then all commands *except* the
ones listed have their candidates sorted.

Note that this variable overrides the sorting options of Ivy,
unless `ivy-prescient-enable-sorting' is nil in which case it has
no effect."
  :group 'prescient
  :type '(choice (boolean :tag "Unconditional")
                 (repeat :tag "Whitelist" function)
                 (cons (const :not) (repeat :tag "Blacklist" function))))

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

(defun ivy-prescient-re-builder (query)
  "Generate an Ivy-formatted regexp list for the given QUERY string.
This is for use in `ivy-re-builders-alist'."
  (setq ivy--subexps 0)
  (save-match-data
    (or
     (mapcar
      (lambda (regexp)
        (setq ivy--subexps (max ivy--subexps (regexp-opt-depth regexp)))
        (cons regexp t))
      (prescient-filter-regexps
       query
       (if ivy-prescient-retain-classic-highlighting
           'all
         'with-group)))
     ;; For some reason, Ivy doesn't seem to like to be given an empty
     ;; list of regexps. Instead, it wants an empty string.
     "")))

(defvar ivy-prescient--old-re-builder nil
  "Previous default value in `ivy-re-builders-alist'.")

(defun ivy-prescient--elements-ensure (element)
  "Ensure that the type of `ELEMENT' is acceptable to prescient.
In addition to string, ivy accepts many types of data, so some
processing is needed to ensure that `prescient' can handle them."
  (if (stringp element)
      element
    (if (consp element)
        (ivy-prescient--elements-ensure (car element))
      (symbol-name element))))

(defun ivy-prescient-sort-function (c1 c2)
  "Comparison function that uses prescient.el to sort candidates.
This is for use in `ivy-sort-functions-alist'. C1 and C2 are
arbitrary candidates to be compared; they may be strings or cons
cells whose cars are strings, or symbols."
  (setq c1 (ivy-prescient--elements-ensure c1))
  (setq c2 (ivy-prescient--elements-ensure c2))
  (prescient-sort-compare c1 c2))

(defun ivy-prescient-remember (candidate)
  "Invokes `prescient-remember' with additional normalization for Ivy.
CANDIDATE is as in `prescient-remember' (which see)."
  (setq candidate (ivy-prescient--elements-ensure candidate))
  (prescient-remember candidate))

(defvar ivy-prescient--old-ivy-sort-functions-alist nil
  "Previous values from `ivy-sort-functions-alist'.
When `ivy-prescient-mode' is disabled, all of the elements of
this alist are used to update `ivy-sort-functions-alist'.")

(defvar ivy-prescient--old-ivy-sort-matches-completion-in-region-function nil
  "Previous value for sorting `completion-in-region' results.
This is the value that was associated to
`ivy-completion-in-region' in `ivy-sort-matches-functions-alist'.")

(defvar ivy-prescient--old-initial-inputs-alist nil
  "Previous value of `ivy-initial-inputs-alist'.")

(defun ivy-prescient--wrap-action (action)
  "Wrap an action for use in `ivy-read'.
ACTION is the original action, a function. Return a new function
that also invokes `prescient-remember'."
  (if (or (bound-and-true-p ivy-marked-candidates)
          (not (eq 'ivy-prescient-sort-function
                   (let ((sort (ivy-state-sort ivy-last))
                         (coll (ivy-state-collection ivy-last)))
                     (cond ((functionp sort) sort)
                           ((or sort (eq coll #'read-file-name-internal))
                            (ivy--sort-function coll)))))))
      action
    (let ((dir ivy--directory))
      (lambda (x)
        (let ((cand x))
          (when (listp cand) (setq cand (car x)))
          (when dir (setq cand (file-relative-name cand dir)))
          (ivy-prescient-remember cand))
        (funcall action x)))))

(defun ivy-prescient--remember-directory (success)
  "Remember the directory we just entered when SUCCESS."
  (when success
    (ivy-prescient-remember
     (file-name-as-directory
      (file-name-nondirectory
       (directory-file-name ivy--directory)))))
  success)

(defun ivy-prescient--enable-sort-commands (args)
  "Enable sorting of `ivy-prescient-sort-commands'.
If the `:caller' in ARGS should be sorted according to
`ivy-prescient-sort-commands', then `:sort' is enabled even if
wasn't in the call to `ivy-read'."
  (when (or (and (symbolp ivy-prescient-sort-commands)
                 ivy-prescient-sort-commands)
            (and (listp ivy-prescient-sort-commands)
                 (if (eq (car ivy-prescient-sort-commands) :not)
                     (not (memq (plist-get args :caller)
                                (cdr ivy-prescient-sort-commands)))
                   (memq (plist-get args :caller)
                         ivy-prescient-sort-commands))))
    ;; Put it at the end so it doesn't override a value that's already
    ;; there. (I.e., you can explicitly pass `:sort nil' to disable
    ;; sorting.)
    (setq args (append args '(:sort t))))
  args)

;;;###autoload
(define-minor-mode ivy-prescient-mode
  "Minor mode to use prescient.el in Ivy menus."
  :global t
  :group 'prescient
  (if ivy-prescient-mode
      (progn
        (ivy-prescient-mode -1)
        (setq ivy-prescient-mode t)
        (when ivy-prescient-enable-filtering
          (cl-shiftf ivy-prescient--old-re-builder
                     (alist-get t ivy-re-builders-alist)
                     #'ivy-prescient-re-builder)
          (cl-shiftf ivy-prescient--old-initial-inputs-alist
                     ivy-initial-inputs-alist
                     nil))
        (when ivy-prescient-enable-sorting
          ;; Not sure if `map-apply' (note that `map-do' is not
          ;; available before Emacs 26) handles mutation of alist
          ;; during iteration. Use `map-keys' plus `dolist' to be
          ;; safe.
          (dolist (caller (map-keys ivy-sort-functions-alist))
            (when (memq (alist-get caller ivy-sort-functions-alist)
                        '(ivy-string< ivy-sort-file-function-default))
              ;; Use `ignore' to silence byte-compiler. We only use
              ;; the setter, not the getter.
              (ignore
               (cl-shiftf
                (alist-get caller ivy-prescient--old-ivy-sort-functions-alist)
                (alist-get caller ivy-sort-functions-alist)
                #'ivy-prescient-sort-function))))
          (cl-shiftf
           ivy-prescient--old-ivy-sort-matches-completion-in-region-function
           (alist-get #'ivy-completion-in-region
                      ivy-sort-matches-functions-alist)
           nil)
          (advice-add #'ivy-read :filter-args
                      #'ivy-prescient--enable-sort-commands)
          (advice-add #'ivy--directory-enter :filter-return
                      #'ivy-prescient--remember-directory)
          (advice-add #'ivy--get-action :filter-return
                      #'ivy-prescient--wrap-action)))
    (when (equal (alist-get t ivy-re-builders-alist)
                 #'ivy-prescient-re-builder)
      (setf (alist-get t ivy-re-builders-alist)
            ivy-prescient--old-re-builder))
    (map-apply (lambda (caller function)
                 (when (equal (alist-get caller ivy-sort-functions-alist)
                              #'ivy-prescient-sort-function)
                   (setf (alist-get caller ivy-sort-functions-alist)
                         function)))
               ivy-prescient--old-ivy-sort-functions-alist)
    (setq ivy-prescient--old-ivy-sort-functions-alist nil)
    (unless (alist-get #'ivy-completion-in-region
                       ivy-sort-matches-functions-alist)
      (setf (alist-get #'ivy-completion-in-region
                       ivy-sort-matches-functions-alist)
            ivy-prescient--old-ivy-sort-matches-completion-in-region-function))
    (dolist (pair (reverse ivy-prescient--old-initial-inputs-alist))
      (unless (alist-get (car pair) ivy-initial-inputs-alist)
        (setf (alist-get (car pair) ivy-initial-inputs-alist) (cdr pair))))
    (advice-remove #'ivy-read #'ivy-prescient--enable-sort-commands)
    (advice-remove #'ivy--directory-enter #'ivy-prescient--remember-directory)
    (advice-remove #'ivy--get-action #'ivy-prescient--wrap-action)))

;;;; Closing remarks

(provide 'ivy-prescient)

;;; ivy-prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; checkdoc-verb-check-experimental-flag: nil
;; End:
