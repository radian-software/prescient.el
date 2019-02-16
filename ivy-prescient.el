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

(defcustom ivy-prescient-filter-method-keys
  '(("C-c C-r" . ((literal+initialism . regexp)
                  (regexp             . literal+initialism))))
  "Bindings for `ivy-minibuffer-map' for switching filter methods.
This is an alist where the keys are key sequence strings as used
in `kbd', and the values are alists whose own keys and values are
values for `prescient-filter-method'. When a key sequence is
typed, the link in the alist corresponding to the current
`prescient-filter-method' is identified, and the cdr of that link
is used to find the new value for `prescient-filter-method'.

The resulting changes to `prescient-filter-method' are local to
the current Ivy session unless
`ivy-prescient-persist-filter-method' is non-nil."
  :group 'prescient
  :type `(alist
          :key-type string
          :value-type (alist
                       :key-type ,prescient--filter-method-custom-type
                       :value-type ,prescient--filter-method-custom-type)))

(defcustom ivy-prescient-persist-filter-method nil
  "Whether changes to `prescient-filter-method' persist between Ivy sessions."
  :group 'prescient
  :type 'boolean)

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

(defcustom ivy-prescient-override-filter-method t
  "Whether to override default ivy re-builder with `prescient-filter-method'.
 Individual collection functions can still be overriden using
`ivy-re-builders-alist'. This will also disable
`ivy-initial-inputs-alist' since the only filter method that
supports it is `regexp'. Changing this variable will not take
effect until `ivy-prescient-mode' has been reloaded."
  :group 'prescient
  :type 'boolean)

(defcustom ivy-prescient-adaptive-sorting t
  "Whether to use adaptive sorting in ivy collections.
Invividual collection functions can still be overriden using
`ivy-sort-functions-alist'. Note that
`ivy-prescient-sort-commands' enables sorting of collections not
normally supported. Changing this variable will not take effect
until `ivy-prescient-mode' has been reloaded."
  :group 'prescient
  :type 'boolean)

;;;; Utility functions

(defun ivy-prescient--make-filter-method-keymap ()
  "Make a keymap from `ivy-prescient-filter-method-keys'."
  (let ((keymap (make-sparse-keymap)))
    (prog1 keymap
      (map-apply
       (lambda (key-string sub-alist)
         (define-key
           keymap (kbd key-string)
           (lambda ()
             (interactive)
             (when-let ((new-filter-method
                         (alist-get prescient-filter-method sub-alist)))
               (setq prescient-filter-method new-filter-method)))))
       ivy-prescient-filter-method-keys))))

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

(defun ivy-prescient--wrap-action (caller action)
  "Wrap an action for use in `ivy-read'.
CALLER is the `:caller' argument to `ivy-read', and ACTION is the
original action, a function. Return a new function that also
invokes `prescient-remember'."
  (lambda (result)
    ;; Same as in `ivy-prescient-sort-function', we have to account
    ;; for candidates which are lists by taking their cars. Make sure
    ;; to do this only for the call to `prescient-remember', and not
    ;; for the actual action, though. See
    ;; https://github.com/raxod502/prescient.el/issues/12.
    (let ((result result))
      (when (listp result)
        (setq result (car result)))
      (unless (memq caller ivy-prescient-excluded-commands)
        (prescient-remember result)))
    (when action
      (funcall action result))))

(cl-defun ivy-prescient-read
    (ivy-read prompt collection &rest rest &key action caller keymap
              &allow-other-keys)
  "Delegate to `ivy-read', handling persistence and sort customization.
If the `:caller' passed to `ivy-read' is a member of
`ivy-prescient-sort-commands', then `:sort' is unconditionally
enabled. Also, `:action' is modified so that the selected
candidate is passed to `prescient-remember'. Finally, `:keymap'
is updated according to the value of
`ivy-prescient-filter-method-keys'.

This is an `:around' advice for `ivy-read'. IVY-READ is the
original definition of `ivy-read', and PROMPT, COLLECTION are the
same as in `ivy-read'. REST is the list of keyword arguments, and
keyword arguments ACTION, CALLER are the same as in `ivy-read'."
  (let ((orig-filter-method prescient-filter-method))
    (unwind-protect
        (apply ivy-read prompt collection
               (append `(:action
                         ,(if (or (null action) (functionp action))
                              (ivy-prescient--wrap-action caller action)
                            (mapcar
                             (lambda (entry)
                               (if (listp entry)
                                   (cl-destructuring-bind
                                       (key fun . rest) entry
                                     (apply #'list key
                                            (ivy-prescient--wrap-action
                                             caller fun)
                                            rest))
                                 entry))
                             action))
                         ,@(when (memq caller ivy-prescient-sort-commands)
                             `(:sort t))
                         :keymap
                         ,(let ((filter-method-keymap
                                 (ivy-prescient--make-filter-method-keymap)))
                            (if keymap
                                (make-composed-keymap
                                 filter-method-keymap
                                 keymap)
                              filter-method-keymap)))
                       rest))
      (unless ivy-prescient-persist-filter-method
        (setq prescient-filter-method orig-filter-method)))))

;;;###autoload
(define-minor-mode ivy-prescient-mode
  "Minor mode to use prescient.el in Ivy menus."
  :global t
  :group 'prescient
  (if ivy-prescient-mode
      (progn
        (when ivy-prescient-override-filter-method
          (setq ivy-prescient--old-re-builder
                (alist-get t ivy-re-builders-alist))
          (setf (alist-get t ivy-re-builders-alist)
                #'ivy-prescient-re-builder)
          (setq ivy-prescient--old-initial-inputs-alist ivy-initial-inputs-alist)
          (setq ivy-initial-inputs-alist nil))
        (when ivy-prescient-adaptive-sorting
          (setq ivy-prescient--old-ivy-sort-function
                (alist-get t ivy-sort-functions-alist))
          (setf (alist-get t ivy-sort-functions-alist)
                #'ivy-prescient-sort-function)
          (setq ivy-prescient--old-ivy-sort-file-function
                (alist-get #'read-file-name-internal ivy-sort-functions-alist))
          (setf (alist-get #'read-file-name-internal ivy-sort-functions-alist)
                #'ivy-prescient-sort-file-function))
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
    (unless ivy-initial-inputs-alist
      (dolist (pair (reverse ivy-prescient--old-initial-inputs-alist))
        (setf (alist-get (car pair) ivy-initial-inputs-alist) (cdr pair))))
    (advice-remove #'ivy-read #'ivy-prescient-read)))

;;;; Closing remarks

(provide 'ivy-prescient)

;;; ivy-prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; checkdoc-verb-check-experimental-flag: nil
;; End:
