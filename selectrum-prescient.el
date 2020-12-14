;;; selectrum-prescient.el --- Selectrum integration -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 8 Dec 2019
;; Package-Requires: ((emacs "25.1") (prescient "5.0") (selectrum "1.0"))
;; SPDX-License-Identifier: MIT
;; Version: 5.0

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

;;;; Minor mode

(defun selectrum-prescient--preprocess (candidates)
  "Sort CANDIDATES, unless `selectrum-should-sort-p' is nil."
  (when selectrum-should-sort-p
    (setq candidates (prescient-sort candidates)))
  candidates)

(defvar selectrum-prescient--old-preprocess-function nil
  "Previous value of `selectrum-preprocess-candidates-function'.")

(defvar selectrum-prescient--old-refine-function nil
  "Previous value of `selectrum-refine-candidates-function'.")

(defun selectrum-prescient--remember (candidate &rest _)
  "Remember CANDIDATE in prescient.el.
For use on `selectrum-candidate-selected-hook'."
  (prescient-remember candidate))

(defun selectrum-prescient--highlight (input candidates)
  "According to INPUT, return list of propertized CANDIDATES."
  (let ((regexps (prescient-filter-regexps input 'with-groups)))
    (save-match-data
      (mapcar
       (lambda (candidate)
         (setq candidate (copy-sequence candidate))
         (prog1 candidate
           (dolist (regexp regexps)
             (when (string-match regexp candidate)
               (put-text-property
                (match-beginning 0) (match-end 0)
                'face 'selectrum-primary-highlight candidate)
               (cl-loop
                for (start end)
                on (cddr (match-data))
                by #'cddr
                do (when (and start end)
                     (put-text-property
                      start end
                      'face 'selectrum-secondary-highlight candidate)))))))
       candidates))))

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

           ;; Otherwise, if we need to add or remove from the list,
           ;; make sure it's actually a list and not just a symbol.
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
                       (delq ',filter-type prescient-filter-method)
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
(selectrum-prescient-create-and-bind-toggle-command prefix "p")
(selectrum-prescient-create-and-bind-toggle-command regexp "r")

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
        (setq selectrum-prescient--old-refine-function
              selectrum-refine-candidates-function)
        (setq selectrum-refine-candidates-function
              #'prescient-filter)
        (setq selectrum-prescient--old-preprocess-function
              selectrum-preprocess-candidates-function)
        (setq selectrum-preprocess-candidates-function
              #'selectrum-prescient--preprocess)
        (setq selectrum-prescient--old-highlight-function
              selectrum-highlight-candidates-function)
        (setq selectrum-highlight-candidates-function
              #'selectrum-prescient--highlight)
        (add-hook 'selectrum-candidate-selected-hook
                  #'selectrum-prescient--remember)
        (add-hook 'selectrum-candidate-inserted-hook
                  #'selectrum-prescient--remember)
        (define-key selectrum-minibuffer-map
          (kbd "M-s") selectrum-prescient-toggle-map))
    (when (eq selectrum-refine-candidates-function
              #'prescient-filter)
      (setq selectrum-refine-candidates-function
            selectrum-prescient--old-refine-function))
    (when (eq selectrum-preprocess-candidates-function
              #'selectrum-prescient--preprocess)
      (setq selectrum-preprocess-candidates-function
            selectrum-prescient--old-preprocess-function))
    (when (eq selectrum-highlight-candidates-function
              #'selectrum-prescient--highlight)
      (setq selectrum-highlight-candidates-function
            selectrum-prescient--old-highlight-function))
    (remove-hook 'selectrum-candidate-selected-hook
                 #'selectrum-prescient--remember)
    (remove-hook 'selectrum-candidate-inserted-hook
                 #'selectrum-prescient--remember)
    (when (equal (lookup-key selectrum-minibuffer-map (kbd "M-s"))
                 selectrum-prescient-toggle-map)
      (define-key selectrum-minibuffer-map (kbd "M-s") nil))))

;;;; Closing remarks

(provide 'selectrum-prescient)

;;; selectrum-prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
