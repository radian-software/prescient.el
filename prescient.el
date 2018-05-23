;;; prescient.el --- Better sorting and filtering. -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 7 Aug 2017
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0

;;; Commentary:

;; prescient.el is a general-purpose library for sorting and filtering
;; candidates.

;; The algorithm of prescient.el is very simple. You enter a query, or
;; multiple queries separated by spaces (two spaces match a literal
;; space), and each query filters the candidates by matching either a
;; substring (e.g. "scient" matches "prescient-separator-chars") or
;; initialism (e.g. "psc" also matches the same). Then, candidates are
;; sorted to prioritize recently chosen candidates, followed by
;; frequently chosen candidates, with the remaining candidates sorted
;; by length.

;; prescient.el aims to replace a number of other packages, including
;; IDO, Smex, Flx, Historian, and Company-Statistics. It also replaces
;; the sorting and filtering functionalities of completion frameworks
;; such as Helm and Ivy.

;; To use prescient.el for Ivy, see ivy-prescient.el. To use
;; prescient.el for Company, see company-prescient.el. In either case,
;; you will most likely want your usage statistics to be saved across
;; Emacs sessions; to do this, enable `prescient-persist-mode' in your
;; init-file or interactively.

;; For more information, see https://github.com/raxod502/prescient.el.

;;; Code:

;;;; Libraries

(require 'cl-lib)
(require 'subr-x)

;;;; User options

(defgroup prescient nil
  "Simple but effective candidate sorting by usage."
  :group 'convenience
  :prefix "prescient-")

(defcustom prescient-separator-chars "-_/+[:space:]"
  "Regexp character class for word separators.
When brackets are placed around this string, it should form a
valid regexp."
  :group 'prescient
  :type 'string)

(defcustom prescient-history-length 100
  "Number of recently chosen candidates that will be remembered."
  :group 'prescient
  :type 'number)

(defcustom prescient-frequency-decay 0.997
  "Rate at which frequently chosen candidates will be forgotten.
Every time a candidate is selected, all candidates are multiplied
by this factor. See also `prescient-frequency-threshold'."
  :group 'prescient
  :type 'number)

(defcustom prescient-frequency-threshold 0.05
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

(defvar prescient-history (make-hash-table :test 'equal)
  "Hash table of recently chosen candidates.
The keys are candidates as strings and the values are 0-based
indices, less than `prescient-history-length'. The number of
values will be at most `prescient-history-length'.")

(defvar prescient-frequency (make-hash-table :test 'equal)
  "Hash table of frequently chosen candidates.
The keys are candidates as strings and the values are
frequencies (floating-point numbers). Frequencies will be at
least `prescient-frequency-threshold'.")

(defvar prescient-cache-loaded nil
  "Non-nil if prescient.el data was loaded from `prescient-save-file'.
Even if the load failed, this variable is still set to non-nil
when `prescient-load' is called.")

(defvar prescient-serial-number 0
  "Number of times `prescient-remember' has been called.

This is used to determine which set of changes to the save file
should \"win\" when two concurrent Emacs sessions want to modify
it.")

;;;; Persistence

(defun prescient-cache-version (_version)
  "Throw an error.
This function was used in previous versions of prescient.el. If
it is called while loading `prescient-save-file', then the save
file has too old of a version."
  (error "`prescient-save-file' has version <= 2"))

(defvar prescient-cache-version 5
  "Current version number of `prescient-save-file' format.")

(defvar prescient-cache-callback #'ignore
  "Callback function called by loading `prescient-save-file'.
A `funcall' to this variable is written to `prescient-save-file'.
The function may produce errors; they will be ignored.

Usually this variable is dynamically bound to another value while
`prescient-save-file' is loaded.")

(defun prescient-load-save-file ()
  "Load `prescient-save-file', ignoring errors."
  (let ((load-source-file-function nil))
    (ignore-errors
      (load prescient-save-file 'noerror 'nomessag))))

(defun prescient-load ()
  "Read data from `prescient-save-file'."
  (interactive)
  (cl-letf ((prescient-cache-callback
             (lambda (&rest args)
               (when (equal (plist-get args :version) prescient-cache-version)
                 (setq prescient-history (plist-get args :history))
                 (setq prescient-frequency (plist-get args :frequency))
                 (setq prescient-serial-number
                       (plist-get args :serial-number))))))
    (prescient-load-save-file))
  (setq prescient-cache-loaded t))

(defun prescient-save ()
  "Write data to `prescient-save-file'."
  (cl-letf* ((saved-serial-number nil)
             (prescient-cache-callback
              (lambda (&rest args)
                (when (equal (plist-get args :version) prescient-cache-version)
                  (setq saved-serial-number
                        (plist-get args :serial-number))))))
    (prescient-load-save-file)
    (when (or (not (numberp saved-serial-number))
              (>= prescient-serial-number saved-serial-number))
      (make-directory (file-name-directory
                       (expand-file-name prescient-save-file))
                      'parents)
      (with-temp-file prescient-save-file
        (print
         `(funcall prescient-cache-callback
                   :version ',prescient-cache-version
                   :history ',prescient-history
                   :frequency ',prescient-frequency
                   :serial-number ',prescient-serial-number)
         (current-buffer))))))

(define-minor-mode prescient-persist-mode
  "Minor mode to persist prescient.el statistics to `prescient-save-file'."
  :global t
  (if prescient-persist-mode
      (add-hook 'kill-emacs-hook #'prescient-save)
    (remove-hook 'kill-emacs-hook #'prescient-save)))

;;;; Utility functions

(defun prescient-split-query (query)
  "Split QUERY string into sub-queries.
The query is split on spaces, but a sequence of two or more
spaces has one space removed and is treated literally rather than
as a sub-query delimiter."
  (if (string-match-p "\\` *\\'" query)
      ;; If string is zero or one spaces, then we match everything.
      ;; Return an empty subquery list.
      (unless (<= (length query) 1)
        ;; Otherwise, the number of spaces should be reduced by one.
        (substring query 1))
    ;; Trim off a single space from the beginning and end, if present.
    ;; Otherwise, they would generate empty splits and cause us to
    ;; match literal whitespace.
    (setq query (replace-regexp-in-string
                 "\\` ?\\(.*?\\) ?\\'" "\\1" query 'fixedcase))
    (let ((splits (split-string query " "))
          (subquery "")
          (token-found nil)
          (subqueries nil))
      (dolist (split splits)
        ;; Check for empty split, meaning two consecutive spaces in
        ;; the original query.
        (if (string-empty-p split)
            (progn
              ;; Consecutive spaces mean literal spaces in the
              ;; subquery under construction.
              (setq subquery (concat subquery " "))
              ;; If we get a non-empty split, append it to the
              ;; subquery rather than parsing it as another subquery.
              (setq token-found nil))
          ;; Possibly add the collected string as a new subquery.
          (when token-found
            (push subquery subqueries)
            (setq subquery ""))
          ;; Either start a new subquery or append to the existing one
          ;; (in the case of previously seeing an empty split).
          (setq subquery (concat subquery split))
          ;; If another non-empty split is found, it's a separate
          ;; subquery.
          (setq token-found t)))
      ;; Check if we hit the end of the string while still
      ;; constructing a subquery, and handle.
      (unless (string-empty-p subquery)
        (push subquery subqueries))
      ;; We added the subqueries in reverse order.
      (nreverse subqueries))))

(defun prescient-initials-regexp (query &optional with-groups)
  "Return a regexp matching QUERY as an initialism.
This means that the regexp will only match a given string if
QUERY is a substring of the initials of the string. The locations
of initials are determined using `prescient-separator-chars'.

If WITH-GROUPS is non-nil, enclose all literal text in capture
groups, so that it can be determined which parts of a matched
candidate should be highlighted."
  (concat (format "\\(?:^\\|[%s]\\)" prescient-separator-chars)
          (mapconcat (lambda (char)
                       (let ((r (regexp-quote (char-to-string char))))
                         (if with-groups
                             (format "\\(%s\\)" r)
                           r)))
                     query
                     (format "[^%s]*[%s]+"
                             prescient-separator-chars
                             prescient-separator-chars))))

;;;; Sorting and filtering

(defun prescient-filter-regexps (query &optional with-groups)
  "Convert QUERY to list of regexps.
Each regexp must match the candidate in order for a candidate to
match the QUERY.

If WITH-GROUPS is non-nil, enclose all literal text in capture
groups, so that it can be determined which parts of a matched
candidate should be highlighted."
  (mapcar
   (lambda (subquery)
     (format "%s\\|%s"
             (let ((r (regexp-quote subquery)))
               (if with-groups
                   (format "\\(%s\\)" r)
                 r))
             (prescient-initials-regexp subquery with-groups)))
   (prescient-split-query query)))

(defun prescient-filter (query candidates)
  "Use QUERY to filter list of CANDIDATES.
Split the query using `prescient-split-query'. Each candidate
must match each subquery, either using substring or initialism
matching. Discard any that do not, and return the resulting
list. Do not modify CANDIDATES."
  (let ((regexps (prescient-filter-regexps query)))
    (save-match-data
      (cl-remove-if-not
       (lambda (candidate)
         (unless (stringp candidate)
           (setq candidate (format "%s" candidate)))
         (cl-every
          (lambda (regexp)
            (string-match regexp candidate))
          regexps))
       candidates))))

(defun prescient-sort-compare (c1 c2)
  "Compare candidates C1 and C2 by usage and length.

If `prescient-persist-mode' is enabled, then ensure that usage
data has been loaded from `prescient-save-file' before comparing.
Loading will only be attempted once, not before every
comparison."
  (unless (stringp c1)
    (setq c1 (format "%s" c1)))
  (unless (stringp c2)
    (setq c2 (format "%s" c2)))
  (when (and prescient-persist-mode (not prescient-cache-loaded))
    (prescient-load))
  (let ((p1 (gethash c1 prescient-history prescient-history-length))
        (p2 (gethash c2 prescient-history prescient-history-length)))
    (or (< p1 p2)
        (and (= p1 p2)
             (let ((f1 (gethash c1 prescient-frequency 0))
                   (f2 (gethash c2 prescient-frequency 0)))
               (or (> f1 f2)
                   (and (= f1 f2)
                        (< (length c1)
                           (length c2)))))))))

(defun prescient-sort (candidates)
  "Sort CANDIDATES using the data in `prescient-frequency'.
Return the sorted list. The original is modified destructively."
  (sort candidates #'prescient-sort-compare))

;;;; Candidate selection

(defun prescient-remember (candidate)
  "Record CANDIDATE in `prescient-history' and `prescient-frequency'."
  ;; Convert to plain string.
  (unless (stringp candidate)
    (setq candidate (format "%s" candidate)))
  (setq candidate (substring-no-properties candidate))
  ;; Add to `prescient-history'.
  (let ((this-pos (gethash
                   candidate prescient-history prescient-history-length)))
    ;; If the candidate was already in the history, then prepare for
    ;; moving it to the front by incrementing the indices of other
    ;; candidates.
    (maphash
     (lambda (other-candidate other-pos)
       (cond
        ;; If the other candidate came earlier in the history, then
        ;; increment its index.
        ((< other-pos this-pos)
         (puthash other-candidate (1+ other-pos) prescient-history))
        ;; Else, if it's already past the end of the history (this
        ;; would happen if `prescient-history-length' were decreased),
        ;; or if it's at the very end and a new candidate was added,
        ;; then remove it from the history.
        ((or (>= other-pos prescient-history-length)
             (and (= other-pos (1- prescient-history-length))
                  (= this-pos prescient-history-length)))
         (remhash other-candidate prescient-history))))
     prescient-history)
    ;; Now add the new candidate to the beginning.
    (puthash candidate 0 prescient-history))
  ;; Add to `prescient-frequency'.
  (puthash candidate (1+ (gethash candidate prescient-frequency 0))
           prescient-frequency)
  ;; Remove old entries from `prescient-frequency'.
  (maphash (lambda (cand old-freq)
             (let ((new-freq (* old-freq prescient-frequency-decay)))
               (if (< new-freq prescient-frequency-threshold)
                   (remhash cand prescient-frequency)
                 (puthash cand new-freq prescient-frequency))))
           prescient-frequency)
  ;; Update serial number.
  (cl-incf prescient-serial-number))

;;;; Closing remarks

(provide 'prescient)

;;; prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
