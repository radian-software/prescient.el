;;; prescient.el --- Smart/stupid candidate sorting. -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 7 Aug 2017
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0

;;; Commentary:

;; This package is under construction.

;;; Code:

;;;; Libraries

(require 'subr-x)

;;;; User options

(defgroup prescient nil
  "Simple but effective candidate sorting by usage."
  :group 'convenience
  :prefix "prescient-")

(defcustom prescient-history-length 10
  "Number of recently chosen candidates that will be remembered.
This is a count per completion context, not a global limit."
  :group 'prescient
  :type 'number)

(defcustom prescient-frequency-decay 0.99
  "Rate at which frequently chosen candidates will be forgotten.
Every time a candidate is selected, all candidates are multiplied
by this factor. See also `prescient-frequency-threshold'."
  :group 'prescient
  :type 'number)

(defcustom prescient-frequency-threshold 0.25
  "Threshold for forgetting about a frequently chosen candidate.
Any candidates with frequencies less than this after a selection
will be discarded. See also `prescient-frequency-decay'."
  :group 'prescient
  :type 'number)

(defcustom prescient-save-file
  (expand-file-name "var/prescient-save.el" user-emacs-directory)
  "File in which to save `prescient-history' and `prescient-frequency'."
  :group 'prescient
  :type 'file)

;;;; Caches

(defvar prescient-history nil
  "List of recently chosen candidates.
The list will be at most `prescient-history-length'.")

(defvar prescient-frequency (make-hash-table :test 'equal)
  "Hash table of frequently chosen candidates.
The keys are candidates (strings or symbols) and the values are
frequencies (floating-point numbers). Frequencies will be at
least `prescient-frequency-threshold'.")

;;;; Utility functions

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
    (nreverse subqueries)))

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

;;;; Sorting and filtering

(defun prescient-filter (query candidates)
  "Use QUERY to filter list of CANDIDATES.
Split the query using `prescient-split-query'. Each candidate
must match each subquery, either using substring or initialism
matching. Discard any that do not, and return the resulting
list. Do not modify CANDIDATES."
  (let* ((subqueries (prescient-split-query query))
         (literal-regexes (mapcar #'regexp-quote subqueries))
         (initials-regexes (mapcar #'prescient-initials-regexp subqueries)))
    (save-match-data
      (cl-remove-if-not
       (lambda (candidate)
         (cl-every
          (lambda (literal-regex initials-regex)
            (or (string-match literal-regex candidate)
                (string-match initials-regex candidate)))
          literal-regexes
          initials-regexes))))))

(defun prescient-sort (candidates)
  "Sort CANDIDATES using the data in `prescient-frequency'.
Return the sorted list. The original is modified destructively."
  (sort candidates
        (lambda (c1 c2)
          (when-let ((f1 (gethash c1 prescient-frequency))
                     (f2 (gethash c2 prescient-frequency)))
            (< f1 f2)))))

;;;; Candidate selection

(defun prescient-remember (candidate)
  "Record CANDIDATE in `prescient-history' and `prescient-frequency'."
  ;; Add to `prescient-history'.
  (cl-pushnew candidate prescient-history :test #'equal)
  ;; Remove old entries from `prescient-history'.
  (when (> (length prescient-history) prescient-history-length)
    (setcdr (nthcdr (1- prescient-history-length) prescient-history) nil))
  ;; Add to `prescient-frequency'.
  (puthash candidate (1+ (gethash candidate prescient-frequency 0))
           prescient-frequency)
  ;; Remove old entries from `prescient-frequency'.
  (maphash (lambda (cand old-freq)
             (let ((new-freq (* old-freq prescient-frequency-decay)))
               (if (< new-freq prescient-frequency-threshold)
                   (remhash cand prescient-frequency)
                 (puthash cand new-freq prescient-frequency))))))

;;;; Persistence

(defun prescient-cache-version (version)
  "Abort reading `prescient-save-file' if VERSION has an unexpected value.
A call to this function is written to `prescient-save-file', and
it is used to detect loading a save file written by an
incompatible verison of prescient.el."
  (unless (equal version 1)
    (throw 'prescient-cache-version-mismatch nil)))

(defun prescient-load ()
  "Read data from `prescient-save-file'.
Set `prescient-history' and `prescient-frequency' accordingly."
  (interactive)
  (catch 'prescient-cache-version-mismatch
    (let ((load-source-file-function nil))
      (load prescient-save-file 'noerror 'nomessage))))

(defun prescient-save ()
  "Write data to `prescient-save-file'.
This uses the values of `prescient-history' and
`prescient-frequency'."
  (make-directory (file-name-directory (expand-file-name prescient-save-file)))
  (with-temp-file prescient-save-file
    (print
     `(progn
        (prescient-cache-version 1)
        (setq prescient-history ',prescient-history)
        (setq prescient-frequency ',prescient-frequency))
     (current-buffer))))

;;;; Closing remarks

(provide 'prescient)

;;; prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
