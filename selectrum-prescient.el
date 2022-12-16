;;; selectrum-prescient.el --- prescient.el + Selectrum -*- lexical-binding: t -*-

;; Copyright (C) 2019-2022 Radian LLC and contributors

;; Author: Radian LLC <contact+prescient@radian.codes>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 8 Dec 2019
;; Package-Requires: ((emacs "25.1") (prescient "6.1.0") (selectrum "3.1"))
;; SPDX-License-Identifier: MIT
;; Version: 6.1.0

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

;;;; Toggling commands
(defun selectrum-prescient--toggle-refresh ()
  "Refresh the Selectrum UI.

This function is added to `prescient--toggle-refresh-functions' by
`selectrum-prescient-mode.'"
  (when selectrum-is-active
    (selectrum-exhibit)))

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
            (kbd "M-s") prescient-toggle-map)
          (add-hook 'prescient--toggle-refresh-functions
                    #'selectrum-prescient--toggle-refresh))
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
                 prescient-toggle-map)
      (define-key selectrum-minibuffer-map (kbd "M-s") nil))
    (remove-hook 'prescient--toggle-refresh-functions
                 #'selectrum-prescient--toggle-refresh)
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
