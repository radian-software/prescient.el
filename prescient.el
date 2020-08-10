;;; prescient.el --- Better sorting and filtering -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 7 Aug 2017
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: MIT
;; Version: 5.0

;;; Commentary:

;; prescient.el is a general-purpose library for sorting and filtering
;; candidates.

;; The algorithm of prescient.el is very simple. You enter a query, or
;; multiple queries separated by spaces (two spaces match a literal
;; space), and each query filters the candidates by matching either a
;; substring (e.g. "scient" matches "prescient-frequency-threshold")
;; or initialism (e.g. "ft" also matches the same). Then, candidates
;; are sorted to prioritize recently chosen candidates, followed by
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

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'cl-lib)
(require 'subr-x)

;;;; User options

(defgroup prescient nil
  "Simple but effective candidate sorting by usage."
  :group 'convenience
  :prefix "prescient-"
  :link '(url-link "https://github.com/raxod502/prescient.el"))

(defcustom prescient-history-length 100
  "Number of recently chosen candidates that will be remembered."
  :type 'number)

(defcustom prescient-frequency-decay 0.997
  "Rate at which frequently chosen candidates will be forgotten.
Every time a candidate is selected, all candidates are multiplied
by this factor. See also `prescient-frequency-threshold'."
  :type 'number)

(defcustom prescient-frequency-threshold 0.05
  "Threshold for forgetting about a frequently chosen candidate.
Any candidates with frequencies less than this after a selection
will be discarded. See also `prescient-frequency-decay'."
  :type 'number)

(defcustom prescient-save-file
  (expand-file-name "var/prescient-save.el" user-emacs-directory)
  "File in which to save usage information.
This only has an effect if `prescient-persist-mode' is enabled."
  :type 'file)

(defcustom prescient-filter-method '(literal regexp initialism)
  "How to interpret prescient.el filtering queries.
Queries are first split on spaces (with two consecutive spaces
standing for a literal space). Then, the candidates are filtered
using each subquery in turn. This variable affects how that
filtering takes place.

Value `literal' means the subquery must be a substring of the
candidate.

Value `regexp' means the subquery is interpreted directly as a
regular expression.

Value `initialism' means the subquery must match a substring of
the initials of the candidate.

Value `fuzzy' means the characters of the subquery must match
some subset of those of the candidate, in the correct order but
not necessarily contiguous.

Value can also be a list of any of the above methods, in which
case each method will be applied in order until one matches.

For backwards compatibility, the value of this variable can also
be `literal+initialism', which equivalent to the list (`literal'
`initialism')."
  :type '(set
          (const :tag "Literal" literal)
          (const :tag "Regexp" regexp)
          (const :tag "Initialism" initialism)
          (const :tag "Fuzzy" fuzzy)))

(defcustom prescient-sort-length-enable t
  "Whether to sort candidates by length.
If non-nil, then candidates with identical recency and frequency
will be sorted by length. If nil, then they will be left in the
order of the original collection.

It might be desirable to set this variable to nil (via
`company-prescient-sort-length-enable') when working with a
Company backend which returns fuzzy-matched results that cannot
usefully be sorted by length (presumably, the backend returns
these results in some already-sorted order)."
  :type 'boolean)

(defcustom prescient-aggressive-file-save nil
  "Whether to save the cache file aggressively.
If non-nil, then write the cache data to `prescient-save-file'
after the cache data is updated by `prescient-remember' when
`prescient-persist-mode' is activated."
  :type 'boolean)

;;;; Caches

(defvar prescient--history (make-hash-table :test 'equal)
  "Hash table of recently chosen candidates.
The keys are candidates as strings and the values are 0-based
indices, less than `prescient-history-length'. The number of
values will be at most `prescient-history-length'.")

(defun prescient--history-as-list ()
  "Return a list of the most recently chosen candidates as strings.
The most recently chosen candidates are at the front of the
list. This function is mostly useful for debugging."
  (let ((history (make-vector prescient-history-length nil)))
    (maphash
     (lambda (cand index)
       (ignore-errors
         (aset history index cand)))
     prescient--history)
    (cl-remove nil (append history nil))))

(defvar prescient--frequency (make-hash-table :test 'equal)
  "Hash table of frequently chosen candidates.
The keys are candidates as strings and the values are
frequencies (floating-point numbers). Frequencies will be at
least `prescient-frequency-threshold'.")

(defvar prescient--cache-loaded nil
  "Non-nil if prescient.el data was loaded from `prescient-save-file'.
Even if the load failed, this variable is still set to non-nil
when `prescient--load' is called.")

(defvar prescient--serial-number 0
  "Number of times `prescient-remember' has been called.

This is used to determine which set of changes to the save file
should \"win\" when two concurrent Emacs sessions want to modify
it.")

;;;; Persistence

(defvar prescient--cache-version 5
  "Current version number of `prescient-save-file' format.")

(defvar prescient-cache-callback #'ignore
  "Callback function called by loading `prescient-save-file'.
A `funcall' to this variable is written to `prescient-save-file'.
The function may produce errors; they will be ignored.

Usually this variable is dynamically bound to another value while
`prescient-save-file' is loaded.")

(defun prescient--load-save-file ()
  "Load `prescient-save-file', ignoring errors."
  (let ((load-source-file-function nil))
    (ignore-errors
      (load prescient-save-file 'noerror 'nomessag))))

(defun prescient--load ()
  "Read data from `prescient-save-file'."
  (interactive)
  (cl-letf ((prescient-cache-callback
             (lambda (&rest args)
               (when (equal (plist-get args :version) prescient--cache-version)
                 (setq prescient--history (plist-get args :history))
                 (setq prescient--frequency (plist-get args :frequency))
                 (setq prescient--serial-number
                       (plist-get args :serial-number))))))
    (prescient--load-save-file))
  (setq prescient--cache-loaded t))

(defun prescient--save ()
  "Write data to `prescient-save-file'."
  (cl-letf* ((saved-serial-number nil)
             (prescient-cache-callback
              (lambda (&rest args)
                (when (equal (plist-get args :version)
                             prescient--cache-version)
                  (setq saved-serial-number
                        (plist-get args :serial-number)))))
             (print-length nil)
             (print-level nil))
    (prescient--load-save-file)
    (when (or (not (numberp saved-serial-number))
              (>= prescient--serial-number saved-serial-number))
      (make-directory (file-name-directory
                       (expand-file-name prescient-save-file))
                      'parents)
      (with-temp-file prescient-save-file
        (print
         `(funcall prescient-cache-callback
                   :version ',prescient--cache-version
                   :history ',prescient--history
                   :frequency ',prescient--frequency
                   :serial-number ',prescient--serial-number)
         (current-buffer))))))

(define-minor-mode prescient-persist-mode
  "Minor mode to persist prescient.el statistics to `prescient-save-file'."
  :global t
  :group 'prescient
  (if prescient-persist-mode
      (progn
        (prescient-persist-mode -1)
        (setq prescient-persist-mode t)
        (add-hook 'kill-emacs-hook #'prescient--save))
    (remove-hook 'kill-emacs-hook #'prescient--save)))

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
        (list (substring query 1)))
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

(defun prescient--with-group (regexp with-group)
  "Wrap REGEXP in a capture group, but only if WITH-GROUP is non-nil."
  (if with-group
      (format "\\(%s\\)" regexp)
    regexp))

(defun prescient--initials-regexp (query &optional with-groups)
  "Return a regexp matching QUERY as an initialism.
This means that the regexp will only match a given string if
QUERY is a substring of the initials of the string.

If WITH-GROUPS is non-nil, enclose the parts of the regexp that
match the actual initials in capture groups, so that the match
data can be used to highlight the initials of the match.

To illustrate, if \"fa\" matches \"find-file-at-point\", then the
entire match will be the text \"file-at\", and there will be two
capture groups matching \"f\" and \"a\"."
  (mapconcat (lambda (char)
               (let ((r (regexp-quote (char-to-string char))))
                 (when with-groups
                   (setq r (format "\\(%s\\)" r)))
                 (format "\\b%s\\w*" r)))
             query
             "\\W*"))

(defun prescient--fuzzy-regexp (query &optional with-groups)
  "Return a regexp for fuzzy-matching QUERY.
This means that the regexp will only match a given string if
all characters in QUERY are present anywhere in the string in
the specified order.

If WITH-GROUPS is non-nil, enclose the parts of the regexp that
match the QUERY characters in capture groups, so that the match
data can be used to highlight the matched substrings."
  (let ((chars (string-to-list query)))
    (concat
     (prescient--with-group
      (regexp-quote
       (char-to-string (car chars)))
      with-groups)
     (mapconcat
      (lambda (char)
        (format "[^%c\n]*?%s" char
                (prescient--with-group
                 (regexp-quote
                  (char-to-string char))
                 with-groups)))
      (cdr chars) ""))))

(defun prescient--prefix-regexp (query &optional with-groups)
  "Return a regexp for matching the beginnings of words in QUERY.
This is similar to the `partial-completion' completion style provided
by Emacs, except that non-word characters are taken literally
\(i.e., one can't glob using \"*\").  Prescient already covers
that case by separating queries with a space.

If WITH-GROUPS is non-nil, enclose the parts of the regexp that
match the QUERY characters in capture groups, so that the match
data can be used to highlight the matched substrings."

  (when (string-match-p "[[:word:]][^[:word:]]" query)
      (prescient--with-group
       (concat "\\<"
               (replace-regexp-in-string
                "[^[:word:]]"
                (lambda (s) (concat "[[:word:]]*" (regexp-quote s)))
                query
                ;; Since quoting the non-word character,
                ;; must replace literally.
                'fixed-case 'literal))
       with-groups)))

;;;; Sorting and filtering

(defun prescient-filter-regexps (query &optional with-groups)
  "Convert QUERY to list of regexps.
Each regexp must match the candidate in order for a candidate to
match the QUERY.

If WITH-GROUPS is non-nil, enclose the initials in initialisms
with capture groups. If it is the symbol `all', additionally
enclose literal substrings with capture groups."
  (mapcar
   (lambda (subquery)
     (string-join
      (cl-remove
       nil
       (mapcar
        (lambda (method)
          (pcase method
            (`literal
             (prescient--with-group
              (regexp-quote subquery)
              (eq with-groups 'all)))
            (`initialism
             (prescient--initials-regexp subquery with-groups))
            (`regexp
             (ignore-errors
               ;; Ignore regexp if it's malformed.
               (string-match-p subquery "")
               subquery))
            (`fuzzy
             (prescient--fuzzy-regexp subquery with-groups))
            (`prefix
             (prescient--prefix-regexp subquery with-groups))))
        (pcase prescient-filter-method
          ;; We support `literal+initialism' for backwards
          ;; compatibility.
          (`literal+initialism '(literal initialism))
          ((and (pred listp) x) x)
          (x (list x))))
       :test #'eq)
      "\\|"))
   (prescient-split-query query)))

(defun prescient-filter (query candidates)
  "Use QUERY to filter list of CANDIDATES.
Split the query using `prescient-split-query'. Each candidate
must match each subquery, either using substring or initialism
matching. Discard any that do not, and return the resulting list.
Do not modify CANDIDATES; always make a new copy of the list."
  (let ((regexps (prescient-filter-regexps query))
        (results nil))
    (save-match-data
      ;; Use named block in case somebody loads `cl' accidentally
      ;; which causes `dolist' to turn into `cl-dolist' which creates
      ;; a nil block implicitly.
      (dolist (candidate candidates)
        (cl-block done
          (dolist (regexp regexps)
            (unless (string-match regexp candidate)
              (cl-return-from done)))
          (push candidate results)))
      (nreverse results))))

(defmacro prescient--sort-compare ()
  "Hack used to cause the byte-compiler to produce faster code.
Note that this macro must be used with several variables in
lexical scope."
  `(progn
     (let* ((p1 (gethash c1 hist len))
            (p2 (gethash c2 hist len)))
       (or (< p1 p2)
           (and (eq p1 p2)
                (let* ((f1 (gethash c1 freq 0))
                       (f2 (gethash c2 freq 0)))
                  (or (> f1 f2)
                      (and (eq f1 f2)
                           len-enable
                           (< (length c1)
                              (length c2))))))))))

(defun prescient-sort-compare (c1 c2)
  "Compare candidates C1 and C2 by usage and length.

If `prescient-persist-mode' is enabled, then ensure that usage
data has been loaded from `prescient-save-file' before comparing.
Loading will only be attempted once, not before every
comparison.

If `prescient-sort-length-enable' is nil, then do not sort by
length."
  (when (and prescient-persist-mode (not prescient--cache-loaded))
    (prescient--load))
  (let ((hist prescient--history)
        (len prescient-history-length)
        (freq prescient--frequency)
        (len-enable prescient-sort-length-enable))
    (prescient--sort-compare)))

(defun prescient-sort (candidates)
  "Sort CANDIDATES using frequency data.
Return the sorted list. The original is modified destructively."
  (when (and prescient-persist-mode (not prescient--cache-loaded))
    (prescient--load))
  ;; Performance optimization revealed that reading dynamic variables
  ;; multiple times was a bottleneck (yes, really), and by reading
  ;; them into lexical variables which are much faster to access, we
  ;; improve the speed of `prescient-sort' on large candidate lists by
  ;; 2x.
  (let ((hist prescient--history)
        (len prescient-history-length)
        (freq prescient--frequency)
        (len-enable prescient-sort-length-enable))
    (sort
     candidates
     (lambda (c1 c2)
       (prescient--sort-compare)))))

;;;; Candidate selection

(defun prescient-remember (candidate)
  "Record CANDIDATE in `prescient--history' and `prescient--frequency'."
  (setq candidate (substring-no-properties candidate))
  ;; Add to `prescient--history'.
  (let ((this-pos (gethash
                   candidate prescient--history prescient-history-length)))
    ;; If the candidate was already in the history, then prepare for
    ;; moving it to the front by incrementing the indices of other
    ;; candidates.
    (maphash
     (lambda (other-candidate other-pos)
       (cond
        ;; If the other candidate came earlier in the history, then
        ;; increment its index.
        ((< other-pos this-pos)
         (puthash other-candidate (1+ other-pos) prescient--history))
        ;; Else, if it's already past the end of the history (this
        ;; would happen if `prescient-history-length' were decreased),
        ;; or if it's at the very end and a new candidate was added,
        ;; then remove it from the history.
        ((or (>= other-pos prescient-history-length)
             (and (= other-pos (1- prescient-history-length))
                  (= this-pos prescient-history-length)))
         (remhash other-candidate prescient--history))))
     prescient--history)
    ;; Now add the new candidate to the beginning.
    (puthash candidate 0 prescient--history))
  ;; Add to `prescient--frequency'.
  (puthash candidate (1+ (gethash candidate prescient--frequency 0))
           prescient--frequency)
  ;; Remove old entries from `prescient--frequency'.
  (maphash (lambda (cand old-freq)
             (let ((new-freq (* old-freq prescient-frequency-decay)))
               (if (< new-freq prescient-frequency-threshold)
                   (remhash cand prescient--frequency)
                 (puthash cand new-freq prescient--frequency))))
           prescient--frequency)
  ;; Update serial number.
  (cl-incf prescient--serial-number)
  ;; Save the cache data.
  (when (and prescient-persist-mode
	     prescient-aggressive-file-save)
    (prescient--save)))

;;;; Closing remarks

(provide 'prescient)

;;; prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
