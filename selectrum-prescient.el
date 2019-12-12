;;; selectrum-prescient.el --- Selectrum integration -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 8 Dec 2019
;; Package-Requires: ((emacs "25.1") (prescient "3.3") (selectrum "0"))
;; Version: 3.3

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

(defvar selectrum-prescient--old-filter-function nil
  "Previous value of `selectrum-candidate-filter-function'.")

(defvar selectrum-prescient--old-sort-function nil
  "Previous value of `selectrum-candidate-sort-function'.")

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
               (cl-block nil
                 (let ((group 1))
                   (while t
                     (if-let ((start (match-beginning group)))
                         (let ((end (match-end group)))
                           (put-text-property
                            start end
                            'face 'selectrum-secondary-highlight candidate))
                       (cl-return))
                     (cl-incf group))))))))
       candidates))))

(defvar selectrum-prescient--old-highlight-function nil
  "Previous value of `selectrum-candidate-highlight-function'.")

;;;###autoload
(define-minor-mode selectrum-prescient-mode
  "Minor mode to use prescient.el in Selectrum menus."
  :global t
  :group 'prescient
  (if selectrum-prescient-mode
      (progn
        (setq selectrum-prescient--old-filter-function
              selectrum-candidate-filter-function)
        (setq selectrum-candidate-filter-function
              #'prescient-filter)
        (setq selectrum-prescient--old-sort-function
              selectrum-candidate-sort-function)
        (setq selectrum-candidate-sort-function
              #'prescient-sort)
        (setq selectrum-prescient--old-highlight-function
              selectrum-candidate-highlight-function)
        (setq selectrum-candidate-highlight-function
              #'selectrum-prescient--highlight)
        (add-hook 'selectrum-candidate-selected-hook
                  #'selectrum-prescient--remember))
    (when (eq selectrum-candidate-filter-function
              #'prescient-filter)
      (setq selectrum-candidate-filter-function
            selectrum-prescient--old-filter-function))
    (when (eq selectrum-candidate-sort-function
              #'prescient-sort)
      (setq selectrum-candidate-sort-function
            selectrum-prescient--old-sort-function))
    (when (eq selectrum-candidate-highlight-function
              #'selectrum-prescient--highlight)
      (setq selectrum-candidate-highlight-function
            selectrum-prescient--old-highlight-function))
    (remove-hook 'selectrum-candidate-selected-hook
                 #'selectrum-prescient--remember)))

;;;; Closing remarks

(provide 'selectrum-prescient)

;;; selectrum-prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
