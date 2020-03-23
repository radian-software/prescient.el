;;; selectrum-prescient.el --- Selectrum integration -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 8 Dec 2019
;; Package-Requires: ((emacs "25.1") (prescient "3.3") (selectrum "1.0"))
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
                  #'selectrum-prescient--remember))
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
                 #'selectrum-prescient--remember)))

;;;; Closing remarks

(provide 'selectrum-prescient)

;;; selectrum-prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
