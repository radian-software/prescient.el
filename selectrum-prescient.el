;;; selectrum-prescient.el --- Selectrum integration -*- lexical-binding: t -*-

;; Copyright (C) 2019-2022 Radian LLC and contributors

;; Author: Radian LLC <contact+prescient@radian.codes>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 8 Dec 2019
;; Package-Requires: ((emacs "25.1") (prescient "5.2.1") (selectrum "3.1"))
;; SPDX-License-Identifier: MIT
;; Version: 5.2.1

;;; Commentary:

;; selectrum-prescient.el provides an interface for using prescient.el
;; to sort and filter candidates in Selectrum menus. To enable its
;; functionality, turn on `selectrum-prescient-mode' in your init-file
;; or interactively.

;; For more information, see https://github.com/raxod502/prescient.el.

;;; Code:

;;;; Libraries

(require 'prescient)
(require 'selectrum)
(require 'subr-x)

(declare-function prescient--highlight-matches "prescient"
                  (input candidates))
(declare-function prescient-sort-full-matches-first "prescient"
                  (candidates regexps ignore-case))
(declare-function prescient-ignore-case-p "prescient"
                  (input))

;;;; Customization

(defgroup selectrum-prescient nil
  "Prescient adapter for Selectrum."
  :group 'convenience
  :prefix "selectrum-prescient"
  :link '(url-link "https://github.com/raxod502/prescient.el"))

(defcustom selectrum-prescient-enable-filtering t
  "Whether to enable filtering by `selectrum-prescient'.
If nil, then `selectrum-prescient-mode' does not change the
filtering behavior of Selectrum from the default. See Selectrum
documentation for how to configure filtering yourself. Changing
this variable will not take effect until
`selectrum-prescient-mode' has been reloaded."
  :group 'selectrum-prescient
  :type 'boolean)

(defcustom selectrum-prescient-enable-sorting t
  "Whether to enable sorting by `selectrum-prescient'.
If nil, then `selectrum-prescient-mode' does not change the
sorting behavior of Selectrum from the default. See Selectrum
documentation for how to configure sorting yourself. Changing
this variable will not take effect until
`selectrum-prescient-mode' has been reloaded."
  :group 'selectrum-prescient
  :type 'boolean)

;;;; Minor mode

(defun selectrum-prescient--preprocess (candidates)
  "Sort CANDIDATES, unless `selectrum-should-sort' is nil."
  (when selectrum-should-sort
    (setq candidates (prescient-sort candidates)))
  candidates)

(defun selectrum-prescient--refine (input candidates)
  "According to INPUT, filter CANDIDATES.

Additionally, if `selectrum-should-sort',
`selectrum-prescient-enable-sorting', and the option
`prescient-sort-full-matches-first' are all non-nil, sort full
matches first."
  (let ((filtered-comps (prescient-filter input candidates))
        ;; TODO?: For some reason, these text properties are lost in
        ;; Selectrum, but not in Vertico and Icomplete Vertical, when
        ;; running `M-x'. This is probably due to how Selectrum
        ;; handles function tables?
        ;;
        ;; Due to the above, we recalculate the values here.
        (regexps (prescient-filter-regexps input))
        (ignore-case (prescient-ignore-case-p input)))
    (if (and selectrum-should-sort
             selectrum-prescient-enable-sorting
             prescient-sort-full-matches-first)
        (prescient-sort-full-matches-first
         filtered-comps regexps ignore-case)
      filtered-comps)))

(defvar selectrum-prescient--old-preprocess-function nil
  "Previous value of `selectrum-preprocess-candidates-function'.")

(defvar selectrum-prescient--old-refine-function nil
  "Previous value of `selectrum-refine-candidates-function'.")

(defun selectrum-prescient--remember (candidate &rest _)
  "Remember CANDIDATE in prescient.el.
For use on `selectrum-candidate-selected-hook'."
  (prescient-remember candidate))

(defvar selectrum-prescient--old-highlight-function nil
  "Previous value of `selectrum-highlight-candidates-function'.")

;;;;; Toggling Commands
(defvar selectrum-prescient-toggle-map (make-sparse-keymap)
  "A keymap of commands for toggling Prescient filters in Selectrum.
Such commands are created and automatically bound in this map by
`selectrum--prescient-create-and-bind-toggle-command'.")

(defmacro selectrum-prescient-create-and-bind-toggle-command
    (filter-type key-string)
  "Create and bind a command to toggle the use of a filter method in Selectrum.

The created command toggles the FILTER-TYPE algorithm on or off
buffer-locally, and doesn't affect the default
behavior (determined by `prescient-filter-method').

FILTER-TYPE is an unquoted symbol that can be used in
`prescient-filter-method'. KEY-STRING is a string that can be
passed to `kbd', whose output will be bound in
`selectrum-prescient-toggle-map' to the created command."
  (let* ((filter-type-name (symbol-name filter-type)))

    `(define-key selectrum-prescient-toggle-map
       (kbd ,key-string)
       (defun ,(intern (concat "selectrum-prescient-toggle-"
                               filter-type-name))
           (arg) ; Arg list
         ,(format
           "Toggle the \"%s\" filter on or off. With ARG, use only this filter.
This toggling only affects filtering in the current Selectrum
buffer. It does not affect the default behavior (determined by
`prescient-filter-method')."  filter-type-name)
         (interactive "P")

         ;; Make `prescient-filter-method' buffer-local in the
         ;; Selectrum buffer. We don't want to accidentally change the
         ;; user's default behavior.
         (make-local-variable 'prescient-filter-method)

         (if arg
             ;; If user provides a prefix argument, set filtering to
             ;; be a list of only one filter type.
             (setq prescient-filter-method '(,filter-type))

           ;; Otherwise, if the current setting is a function,
           ;; evaluate it to get the value.
           (when (functionp prescient-filter-method)
             (setq prescient-filter-method
                   (funcall prescient-filter-method)))

           ;; If we need to add or remove from the list, make sure
           ;; it's actually a list and not just a symbol.
           (when (symbolp prescient-filter-method)
             (setq prescient-filter-method
                   (list prescient-filter-method)))

           (if (equal prescient-filter-method '(,filter-type))
               ;; Make sure the user doesn't accidentally disable all
               ;; filtering.
               (user-error
                "Prescient.el: Can't toggle off only active filter method: %s"
                ,filter-type-name)

             (setq prescient-filter-method
                   (if (memq ',filter-type prescient-filter-method)
                       ;; Even when running `make-local-variable',
                       ;; it seems `delq' might still modify the
                       ;; global value, so we use `remq' here.
                       (remq ',filter-type prescient-filter-method)
                     (cons ',filter-type prescient-filter-method)))))

         ;; After changing `prescient-filter-method', tell the user
         ;; the new value and update Selectrum's display.
         (message "Prescient.el filter is now %s"
                  prescient-filter-method)
         (selectrum-exhibit)))))

(selectrum-prescient-create-and-bind-toggle-command anchored "a")
(selectrum-prescient-create-and-bind-toggle-command fuzzy "f")
(selectrum-prescient-create-and-bind-toggle-command initialism "i")
(selectrum-prescient-create-and-bind-toggle-command literal "l")
(selectrum-prescient-create-and-bind-toggle-command literal-prefix "P")
(selectrum-prescient-create-and-bind-toggle-command prefix "p")
(selectrum-prescient-create-and-bind-toggle-command regexp "r")

(defun selectrum-prescient-toggle-char-fold ()
  "Toggle character folding in the current Selectrum buffer.

See the customizable variable `prescient-use-char-folding'."
  (interactive)
  (setq-local prescient-use-char-folding
              (not prescient-use-char-folding))
  (message "Character folding toggled %s"
           (if prescient-use-char-folding "on" "off"))
  (selectrum-exhibit))

;; This is the same binding used by `isearch-toggle-char-fold'.
(define-key selectrum-prescient-toggle-map (kbd "'")
  #'selectrum-prescient-toggle-char-fold)

(defun selectrum-prescient-toggle-case-fold ()
  "Toggle case folding in the current Selectrum buffer.

If `prescient-use-case-folding' is set to `smart', then this
toggles whether to use smart case folding or no case folding.
Otherwise, this toggles between normal case folding and no case
folding."
  (interactive)
  (setq-local prescient-use-case-folding
              (cond
               (prescient-use-case-folding
                (message "Case folding toggled off")
                nil)
               ((eq (default-toplevel-value 'prescient-use-case-folding)
                    'smart)
                (message "Smart case folding toggled on")
                'smart)
               (t
                (message "Case folding toggled on")
                t)))

  (selectrum-exhibit))

;; This is the same binding used by `isearch-toggle-case-fold'.
(define-key selectrum-prescient-toggle-map (kbd "c")
  #'selectrum-prescient-toggle-case-fold)

;;;###autoload
(define-minor-mode selectrum-prescient-mode
  "Minor mode to use prescient.el in Selectrum menus."
  :global t
  :group 'prescient
  (if selectrum-prescient-mode
      (progn
        ;; Prevent messing up variables if we explicitly enable the
        ;; mode when it's already on.
        (selectrum-prescient-mode -1)
        (setq selectrum-prescient-mode t)
        (when selectrum-prescient-enable-filtering
          (setq selectrum-prescient--old-refine-function
                selectrum-refine-candidates-function)
          (setq selectrum-prescient--old-highlight-function
                selectrum-highlight-candidates-function)
          (setq selectrum-refine-candidates-function
                #'selectrum-prescient--refine)
          (setq selectrum-highlight-candidates-function
                #'prescient--highlight-matches)
          (define-key selectrum-minibuffer-map
            (kbd "M-s") selectrum-prescient-toggle-map))
        (when selectrum-prescient-enable-sorting
          (setq selectrum-prescient--old-preprocess-function
                selectrum-preprocess-candidates-function)
          (setq selectrum-preprocess-candidates-function
                #'selectrum-prescient--preprocess)
          (add-hook 'selectrum-candidate-selected-hook
                    #'selectrum-prescient--remember)
          (add-hook 'selectrum-candidate-inserted-hook
                    #'selectrum-prescient--remember)))
    (when (eq selectrum-refine-candidates-function
              #'selectrum-prescient--refine)
      (setq selectrum-refine-candidates-function
            selectrum-prescient--old-refine-function))
    (when (eq selectrum-highlight-candidates-function
              #'prescient--highlight-matches)
      (setq selectrum-highlight-candidates-function
            selectrum-prescient--old-highlight-function))
    (when (equal (lookup-key selectrum-minibuffer-map (kbd "M-s"))
                 selectrum-prescient-toggle-map)
      (define-key selectrum-minibuffer-map (kbd "M-s") nil))
    (remove-hook 'selectrum-candidate-selected-hook
                 #'selectrum-prescient--remember)
    (remove-hook 'selectrum-candidate-inserted-hook
                 #'selectrum-prescient--remember)
    (when (eq selectrum-preprocess-candidates-function
              #'selectrum-prescient--preprocess)
      (setq selectrum-preprocess-candidates-function
            selectrum-prescient--old-preprocess-function))))

;;;; Closing remarks

(provide 'selectrum-prescient)

;;; selectrum-prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
