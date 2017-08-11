;;; prescient.el --- Smart/stupid candidate sorting. -*- lexical-binding: t -*-

;; Copyright (C) 2017 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 7 Aug 2017

;;; Commentary:

;; This package is under construction.

;;; Code:

;;;; User options

(defvar prescient-history-length 10
  "Number of recently chosen candidates that will be remembered.
This is a count per completion context, not a global limit.")

(defvar prescient-frequency-count 20
  "Number of frequently chosen candidates that will be remembered.
This is a count per completion context, not a global limit.")

(defvar prescient-history-bonus 100
  "Bonus for recently chosen candidates.
The oldest candidate in the recent history will be given this
bonus; the second oldest will be given twice this bonus, and so
on. Thus, the maximum bonus depends on the setting of
`prescient-history-length'.")

(defvar prescient-frequency-bonus 10
  "Bonus for frequently chosen candidates.
The least frequent candidate in the frequency tracker will be
given this bonus; the second least frequent will be given twice
this bonus, and so on. Thus, the maximum bonus depends on the
setting of `prescient-frequency-count'.")

(defvar prescient-prefix-bonus 100
  "Bonus if query matches at beginning of candidate.")

(defvar prescient-word-prefix-bonus 50
  "Bonus if query matches at beginning of word in candidate.")

(defvar prescient-basename-bonus 100
  "Bonus if query matches basename of path rather than a stem component.")

(defvar prescient-in-order-bonus 100
  "Bonus if queries match candidate in order rather than out of order.")

;;;; Main functions

(defun prescient-split-query (query)
  "Split QUERY string into sub-queries.
The query is split on spaces, but a sequence of two or more
spaces has one space removed and is treated literally rather than
as a sub-query delimiter. Also, leading and trailing spaces are
treated literally."
  ;; This algorithm is a little complicated. You can't say it's not
  ;; elegant, though. (Sorry.)
  (let ((splits (split-string query " "))
        (subquery "")
        (token-found nil)
        (subqueries nil))
    (dolist (split splits)
      (if (string-empty-p split)
          (progn
            (setq subquery (concat subquery " "))
            (setq token-found nil))
        (when token-found
          (push subquery subqueries)
          (setq subquery ""))
        (setq subquery (concat subquery split))
        (setq token-found t)))
    (unless (string-empty-p subquery)
      (push subquery subqueries))
    (reverse subqueries)))

(defun prescient-initials-regexp (query)
  "Return a regexp matching QUERY as an initialism.
This means that the regexp will only match a given string if
QUERY is a substring of the initials of the string. The locations
of initials are determined by the location of hyphens, so this
function is not suitable for use with snake case or camel case."
  (concat "\\(^\\|-\\)"
          (mapconcat (lambda (char)
                       (regexp-quote (char-to-string char)))
                     query "[^-]*-+")))

;; 1. check for substring match using regex
;; 2. check for initials match using regex
;; 3. check index and apply prefix bonuses, basename bonus if necessary
;; 4. do this for each query term
;; 5. compare indices, apply in-order bonus if necessary
;; 6. apply history and frequency bonuses (implement this later)

(defun prescient-filter-and-sort (candidates query)
  "Filter and sort a list of CANDIDATES against the given QUERY string.
Return the filtered and sorted list."
  (let ((subqueries (prescient-split-query query))
        (score-alist nil))
    (dolist (candidate candidates)
      ;; FIXME: filter and score candidate
      )
    (setq score-alist (cl-sort score-alist #'< :key #'cdr))
    (mapcar #'car score-alist)))

;;;; Closing remarks

(provide 'prescient)

;;; prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
