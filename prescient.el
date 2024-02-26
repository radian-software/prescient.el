;;; prescient.el --- Better sorting and filtering -*- lexical-binding: t -*-

;; Copyright (C) 2017-2022 Radian LLC and contributors

;; Author: Radian LLC <contact+prescient@radian.codes>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 7 Aug 2017
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: MIT
;; Version: 6.3.0

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

;; Require `char-fold' so that `char-fold-table' gets defined.
;; Otherwise `char-fold-to-regexp' can signal an error.
(require 'char-fold)
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
candidate. Supports char folding.

Value `literal-prefix' means the first subquery must be the
prefix of the candidate and the remaining subqueries must be
prefixes of words in the candidate. Supports char folding.

Value `regexp' means the subquery is interpreted directly as a
regular expression.

Value `initialism' means the subquery must match a substring of
the initials of the candidate.

Value `fuzzy' means the characters of the subquery must match
some subset of those of the candidate, in the correct order but
not necessarily contiguous.

Value `prefix' means the words (substrings of only word
characters) match the beginning of words found in the candidate,
in order, separated by the same non-word characters that separate
words in the query. This is similar to the completion style
`partial'.

Value `anchored' means words are separated by capital letters or
symbols, with capital letters being the start of a new word. This
is similar to `prefix', but allows for less typing.

Value can also be a list of any of the above methods, in which
case each method will be applied in order until one matches.

Value can also be a function which returns any of the allowable
values documented above.

For backwards compatibility, the value of this variable can also
be `literal+initialism', which equivalent to the list (`literal'
`initialism')."
  :type '(set
          (const :tag "Literal" literal)
          (const :tag "Literal Prefix" literal-prefix)
          (const :tag "Regexp" regexp)
          (const :tag "Initialism" initialism)
          (const :tag "Fuzzy" fuzzy)
          (const :tag "Prefix" prefix)
          (const :tag "Anchored" anchored)))

(defcustom prescient-filter-alist
  '((literal . prescient-literal-regexp)
    (literal-prefix . prescient-literal-prefix-regexp)
    (initialism . prescient-initials-regexp)
    (regexp . prescient-regexp-regexp)
    (fuzzy . prescient-fuzzy-regexp)
    (prefix . prescient-prefix-regexp)
    (anchored . prescient-anchored-regexp))
  "An alist of filter methods and their functions.

These symbols can be included in `prescient-filter-method', and
their corresponding functions will be used to create regexps for
matching candidates.

A function should take the sub-query for which it should create a
regexp, and keyword arguments of which there are currently:

    with-group      <bool-value-or-all>
    subquery-number <int-value>

The boolean keyword argument WITH-GROUP describes whether the
function should enclose matched text in a capture group (such as
with `prescient-with-group').  Additionally, if the value is the
symbol `all', then literal substrings should be enclosed in
capture groups.

The integer keyword argument SUBQUERY-NUMBER states the order of
how the input has been split into individual subqueries (starting
with 0).  For example, if the input was \"foo bar baz\" the
function will be called three times:

    (fn \"foo\" :subquery-number 0)
    (fn \"bar\" :subquery-number 1)
    (fn \"baz\" :subquery-number 2)"
  :type '(alist :key-type symbol :value-type function))

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

(defcustom prescient-tiebreaker nil
  "If non-nil, the method used to break ties instead of length.
The value will be called as a function with two candidates that
have the same recency and frequency information, and should
return a number to indicate their relative order (negative for
first < second, zero for first = second, positive for first >
second), where candidates are assumed to sort in ascending order.
You can also use the variable `prescient-query' to access the
original query from the user (but see that variable for
caveats)."
  :type '(choice
          (const :tag "Length" nil)
          (function :tag "Custom function")))

(defcustom prescient-aggressive-file-save nil
  "Whether to save the cache file aggressively.
If non-nil, then write the cache data to `prescient-save-file'
after the cache data is updated by `prescient-remember' when
`prescient-persist-mode' is activated."
  :type 'boolean)

(defcustom prescient-sort-full-matches-first nil
  "Whether to sort fully matched candidates before others.

Prescient can sort by recency, frequency, and candidate length.
With this option, fully matched candidates will be sorted before
partially matched candidates, but candidates in each group will
still be sorted like normal."
  :type 'boolean)

(defcustom prescient-use-char-folding t
  "Whether certain literal filtering methods use character folding.

This affects the `literal' and `literal-prefix' filtering methods.

In Emacs versions 27 or greater, see also the customizable
variables `char-fold-include', `char-fold-exclude', and
`char-fold-symmetric'."
  :type 'boolean)

(defcustom prescient-use-case-folding 'smart
  "Whether filtering methods are case folding.

If t, always use case folding.  If nil, never use case folding.
If `smart' (the default), use case folding only when the query
contains no upper-case letters."
  :type '(choice
          (const :tag "Always" t)
          (const :tag "Never" nil)
          (const :tag "Unless using upper-case letters" smart)))

(defcustom prescient-completion-highlight-matches t
  "Whether the `prescient' completion style should highlight matches.

If `completion-lazy-hilit' is bound and non-nil, then this user
option is ignored in favor of that variable.

See also the faces `prescient-primary-highlight' and
`prescient-secondary-highlight'."
  :type 'boolean)

(define-obsolete-face-alias 'selectrum-primary-highlight
  'prescient-primary-highlight t)
(define-obsolete-face-alias 'selectrum-prescient-primary-highlight
  'prescient-primary-highlight t)
(defface prescient-primary-highlight
  '((t :weight bold))
  "Face used to highlight the parts of candidates that match the input.")

(define-obsolete-face-alias 'selectrum-secondary-highlight
  'prescient-secondary-highlight t)
(define-obsolete-face-alias 'selectrum-prescient-secondary-highlight
  'prescient-secondary-highlight t)
(defface prescient-secondary-highlight
  '((t :inherit prescient-primary-highlight :underline t))
  "Additional face used to highlight parts of candidates.

May be used to highlight parts of candidates that match specific
parts of the input.")

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

(defun prescient-forget (candidate)
  "Remove CANDIDATE from recency and frequency records."
  (interactive
   (list (completing-read "Forget candidate: "
                          ;; Since candidates are shared, select from
                          ;; the table with the most candidates.
                          (if (> (hash-table-size prescient--frequency)
                                 (hash-table-size prescient--history))
                              prescient--frequency
                            prescient--history)
                          nil t)))
  (remhash candidate prescient--history)
  (remhash candidate prescient--frequency))

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
(defun prescient--char-fold-to-regexp (string)
  "Convert STRING to a regexp that handles char folding.
This is the same as `char-fold-to-regexp' but it works around
https://github.com/raxod502/prescient.el/issues/71. The issue
should really be fixed upstream in Emacs, but it looks like that
is not happening anytime soon."
  (let ((regexp (char-fold-to-regexp string)))
    (condition-case _
        (prog1 regexp
          (string-match-p regexp ""))
      (invalid-regexp (regexp-quote string)))))

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

(defun prescient-with-group (regexp with-group)
  "Wrap REGEXP in a capture group, but only if WITH-GROUP is non-nil."
  (if with-group
      (format "\\(%s\\)" regexp)
    regexp))

(defun prescient--prefix-and-pattern (string table pred)
  "Split STRING into prefix and pattern according to TABLE.

The predicate PRED is used to constrain the entries in TABLE."
  (let ((limit (car (completion-boundaries string table pred ""))))
    (cons (substring string 0 limit) (substring string limit))))

(defun prescient-ignore-case-p (input)
  "Whether prescient.el should ignore case considering INPUT.

Filtering can optionally ignore case if `prescient-use-case-folding'
is non-nil.  If `smart', then filtering will not ignore case when
INPUT contains uppercase letters."
  (if (eq prescient-use-case-folding 'smart)
      (let ((case-fold-search nil))
        ;; If using upper-case characters, then don't fold case.
        (not (string-match-p "[[:upper:]]" input)))
    prescient-use-case-folding))

(defun prescient--add-sort-info (candidates &rest properties)
  "Propertize the first candidate in CANDIDATES to save data.

Currently recognized PROPERTIES are:

- `:prescient-match-regexps': The regexps used for filtering.

- `:prescient-all-regexps': All regexps outputted by the filter
  methods, which were used to make PRESCIENT-MATCH-REGEXPS. A
  regexp in this list does not necessarily match the candidates
  by itself.

- `:prescient-ignore-case': Whether prescient ignored case.

- `:prescient-query': Original search query from user.

These properties are identified using keyword symbols.

This information is used by the function
`prescient-sort-full-matches-first'."
  (if (null candidates)
      nil
    (cons (apply #'propertize (car candidates)
                 ;; While PROPERTIES contains all given keys, we
                 ;; explicitly set the properties this way so that
                 ;; we're sure that the properties exist even when
                 ;; they're not given. This makes testing easier
                 ;; and should be helpful for others creating their
                 ;; own sorting functions.
                 ;;
                 ;; Note all passed properties will still get set,
                 ;; this just defaults the standard ones to nil in
                 ;; case they are missing.
                 (cl-flet ((put-get (props sym)
                                    (plist-put props sym
                                               (plist-get props sym))))
                   (thread-first properties
                                 (put-get :prescient-match-regexps)
                                 (put-get :prescient-all-regexps)
                                 (put-get :prescient-ignore-case)
                                 (put-get :prescient-query))))
          (cdr candidates))))

(defun prescient--get-sort-info (candidates)
  "Return a property list of properties added by `prescient-filter'.

`prescient-filter' adds properties to the CANDIDATES that it
filtered for use by the function `prescient-sort-full-matches-first'."
  (cl-loop for cand in candidates
           for props = (text-properties-at 0 cand)
           until (plist-member props :prescient-match-regexps)
           ;; Since we allow other keys in `prescient--add-sort-info',
           ;; just return all properties here.
           finally return props))

(defun prescient--highlight-candidate (regexps case-fold candidate)
  "Highlight text matching REGEXPS and considering CASE-FOLD in CANDIDATE.

Returns a propertized CANDIDATE."
  (setq candidate (copy-sequence candidate))
  (prog1 candidate
    (let ((case-fold-search case-fold))
      (save-match-data
        (dolist (regexp regexps)
          (when (string-match regexp candidate)
            (font-lock-prepend-text-property
             (match-beginning 0) (match-end 0)
             'face 'prescient-primary-highlight
             candidate)
            (cl-loop
             for (start end)
             on (cddr (match-data))
             by #'cddr
             do (when (and start end)
                  (font-lock-prepend-text-property
                   start end
                   'face 'prescient-secondary-highlight
                   candidate)))))))))

(defun prescient--highlight-candidates (input candidates)
  "According to INPUT, highlight the matched sections in CANDIDATES.

INPUT is the string that was used to generate a list of regexps
for filtering. CANDIDATES is the list of filtered candidates,
which should be a list of strings.

Return a list of propertized CANDIDATES."
  (cl-loop with regexps = (prescient-filter-regexps input 'with-group)
           and case-fold-search = (prescient-ignore-case-p input)
           for cand in candidates
           collect (prescient--highlight-candidate regexps case-fold-search
                                                   cand)))

;;;; Regexp Builders

(cl-defun prescient-literal-regexp (query &key with-group
                                          &allow-other-keys)
  "Return a regexp matching QUERY with optional character folding.

If WITH-GROUP is `all', enclose the match in a capture group.

See also the customizable variable `prescient-use-char-folding'."
  (prescient-with-group
   (if prescient-use-char-folding
       (prescient--char-fold-to-regexp query)
     (regexp-quote query))
   (eq with-group 'all)))

(cl-defun prescient-literal-prefix-regexp
    (query &key with-group subquery-number
           &allow-other-keys)
  "Return a regexp matching QUERY with optional character folding.

If WITH-GROUP is `all', enclose the match in a capture group.
Anchor the QUERY at the beginning of the candidate if
SUBQUERY-NUMBER equals 0.

See also the customizable variable `prescient-use-char-folding'."
  (prescient-with-group
   (concat (if (= subquery-number 0)
               ;; 1. subquery => anchor at the beginning of candidate.
               "^"
             ;; Otherwise, just anchor at the beginning of some word
             ;; in the candidate.
             "\\b")
           (if prescient-use-char-folding
               (prescient--char-fold-to-regexp query)
             (regexp-quote query)))
   (eq with-group 'all)))

(cl-defun prescient-initials-regexp (query &key with-group
                                           &allow-other-keys)
  "Return a regexp matching QUERY as an initialism.
This means that the regexp will only match a given string if
QUERY is a substring of the initials of the string.

If WITH-GROUP is non-nil, enclose the parts of the regexp that
match the actual initials in capture groups, so that the match
data can be used to highlight the initials of the match.

To illustrate, if \"fa\" matches \"find-file-at-point\", then the
entire match will be the text \"file-at\", and there will be two
capture groups matching \"f\" and \"a\"."
  (mapconcat (lambda (char)
               (let ((r (regexp-quote (char-to-string char))))
                 (when with-group
                   (setq r (format "\\(%s\\)" r)))
                 (format "\\b%s\\w*" r)))
             query
             "\\W*"))

(cl-defun prescient-regexp-regexp (query &rest _ignore)
  "Unless using the regexp QUERY would return an error, return QUERY."
  (ignore-errors
    ;; Ignore regexp if it's malformed.
    (string-match-p query "")
    query))

(cl-defun prescient-anchored-regexp (query &key with-group
                                           &allow-other-keys)
  "Return a regexp matching QUERY with anchors.
This means uppercase and symbols will be used as begin of words.

If WITH-GROUP is non-nil, enclose the parts of the regexp that
match the actual initials in capture groups, so that the match
data can be used to highlight the initials of the match.

To illustrate, \"FiFiAt\" matches \"find-file-at-point\" with the
entire match being \"file-find-at\" and with three groups
\"find\", \"file\", and \"at\".

A similar match can be achieve with \"fi-fi-at\", or \"FFA\",
or \"find-f-a\"."
  (let ((case-fold-search nil)
        (expr (if with-group
                  "\\(\\b%s\\)[^\\/]*?"
                "\\b%s[^\\/]*?")))
    (replace-regexp-in-string
     "[[:upper:]][[:lower:]]*\\|\\W[[:lower:]]*\\|[[:lower:]]+"
     (lambda (s) (format expr (regexp-quote (downcase s))))
     query
     'fixed-case
     'literal)))

(cl-defun prescient-fuzzy-regexp (query &key with-group
                                        &allow-other-keys)
  "Return a regexp for fuzzy-matching QUERY.
This means that the regexp will only match a given string if all
characters in QUERY are present anywhere in the string in the
specified order.

If WITH-GROUP is non-nil, enclose the parts of the regexp that
match the QUERY characters in capture groups, so that the match
data can be used to highlight the matched substrings."
  (let ((chars (string-to-list query)))
    (concat
     (prescient-with-group
      (regexp-quote
       (char-to-string (car chars)))
      with-group)
     (mapconcat
      (lambda (char)
        (format "[^%c\n]*?%s" char
                (prescient-with-group
                 (regexp-quote
                  (char-to-string char))
                 with-group)))
      (cdr chars) ""))))

(cl-defun prescient-prefix-regexp (query &key with-group
                                         &allow-other-keys)
  "Return a regexp for matching the beginnings of words in QUERY.
This is similar to the `partial-completion' completion style
provided by Emacs, except that non-word characters are taken
literally (i.e., one can't glob using \"*\").  Prescient already
covers that case by separating queries with a space.

If QUERY contains non-word characters, then this matches
greedily. Otherwise, it matches non-greedily. For example,

- \"str-r\" fully matches \"string-repeat\"

- \"re\" does not fully match the word \"repertoire\", only the
  \"re\" at the beginning of the word

- \".g\" fully matches \".git\"

This behavior is meant to work better with the function
`prescient-sort-full-matches-first' and to avoid interpreting an
initialism as a prefix.

If WITH-GROUP is non-nil, enclose the parts of the regexp that
match the QUERY characters in capture groups, so that the match
data can be used to highlight the matched substrings."
  (concat (when (eql 0 (string-match-p "[[:word:]]" query))
            ;; If QUERY begins with a word character, make sure the
            ;; returned regexp doesn't start matching in the middle of
            ;; a word.
            "\\<")
          (replace-regexp-in-string
           "[[:word:]]+"
           ;; Choose whether to wrap sequences of word characters.
           (concat (if with-group
                       "\\\\(\\&\\\\)[[:word:]]*"
                     "\\&[[:word:]]*")
                   (unless (string-match-p "[^[:word:]]" query)
                     "?"))
           ;; Quote non-word characters so that they're taken
           ;; literally.
           (replace-regexp-in-string "[^[:word:]]"
                                     #'regexp-quote
                                     query 'fixed-case 'literal)
           'fixed-case)))

;;;; Sorting and filtering

(defun prescient-filter-regexps (query &optional with-group separated)
  "Convert QUERY to list of regexps.
Each regexp must match the candidate in order for a candidate to
match the QUERY.

If WITH-GROUP is non-nil, enclose the initials in initialisms
with capture groups. If it is the symbol `all', additionally
enclose literal substrings with capture groups.

By default, this function returns a list containing one regexp
for each space-separated sub-query in QUERY, in which each
sub-query's regexp is a combination of the regexps produced by
the filter methods for that sub-query, joined by \"\\|\". If
SEPARATED is non-nil, this function instead returns a list of all
regexps produced by the filter methods, without combining them
into a single regexp for each sub-query."
  (let ((list-of-lists
         (cl-loop
          with filter-methods
          = (pcase (if (functionp prescient-filter-method)
                       (funcall prescient-filter-method)
                     prescient-filter-method)
              ;; We support `literal+initialism' for backwards
              ;; compatibility.
              (`literal+initialism '(literal initialism))
              ((and (pred listp) x) x)
              (x (list x)))
          for subquery in (prescient-split-query query)
          for subquery-number from 0
          collect
          (cl-loop with temp-regexp = nil
                   for method in filter-methods
                   for func = (alist-get method prescient-filter-alist)
                   if (null func)
                   do (message
                       "No function in `prescient-filter-alist' for method: %s"
                       method)
                   else
                   ;; Can't use "for =" here.
                   do (setq temp-regexp
                            (funcall func subquery
                                     :with-group with-group
                                     :subquery-number subquery-number))
                   and if temp-regexp collect temp-regexp end
                   end))))
    (if separated
        (apply #'append list-of-lists)
      (mapcar (lambda (list) (string-join list "\\|"))
              list-of-lists))))

;;;###autoload
(defun prescient-filter (query candidates &optional pred)
  "Use QUERY to filter list of CANDIDATES.

CANDIDATES is a completion table, such as a list of strings
or a function as defined in the Info node
`(elisp)Programmed Completion'.

QUERY is a string containing the sub-queries, which are gotten
using `prescient-split-query'. Each sub-query is used to produce
a regular expression according to the filter methods listed in
`prescient-filter-method'. A candidate must match every regular
expression made from the sub-queries to be included in the list
of returned candidates.

PRED is the predicate used with the completion table, as
described in the above Info node.

This function does not modify CANDIDATES; it always make a new
copy of the list."
  (pcase-let*
      ((`(,prefix . ,pattern)
        (prescient--prefix-and-pattern query candidates pred))
       (completion-regexp-list (prescient-filter-regexps pattern))
       (completion-ignore-case (prescient-ignore-case-p pattern)))

    ;; Add information for `prescient-sort-full-matches-first'. We
    ;; want to add these properties even if we can't modify table
    ;; metadata, since a user might be able to configure their
    ;; completion UI with a custom sorting function that would use
    ;; this info.
    ;;
    ;; There is a question of how to handle prefixes for identifying
    ;; fully matched candidates. Prefixes are used in:
    ;;
    ;; - `completing-read-multiple', as the candidates that have
    ;;   already been selected
    ;;
    ;; - file-name completion, as the directory preceeding the file
    ;;   name, though this seems to only happen when there is no
    ;;   match in some UIs (Icomplete, but not Selectrum)
    ;;
    ;; It might turn out that for file names we need to adjust the
    ;; regexps to be "\(?:QUOTED-PREFIX\)METHOD-REGEXP", but this
    ;; isn't evident yet. We just do the below to be proactive.
    (cl-flet ((maybe-add-prefix (regexps)
                                (if (and (not (string-empty-p prefix))
                                         minibuffer-completing-file-name)
                                    (cl-loop for regexp in regexps
                                             collect (concat
                                                      "\\(?:"
                                                      (regexp-quote prefix)
                                                      "\\)"
                                                      regexp))
                                  regexps)))
      (prescient--add-sort-info
       (all-completions prefix candidates pred)
       :prescient-match-regexps completion-regexp-list
       :prescient-all-regexps (maybe-add-prefix
                               (prescient-filter-regexps pattern nil t))
       :prescient-ignore-case completion-ignore-case
       :prescient-query query))))

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
                           (if tiebreaker
                               (< (funcall tiebreaker c1 c2) 0)
                             (and
                              len-enable
                              (< (length c1)
                                 (length c2))))))))))))

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
        (len-enable prescient-sort-length-enable)
        (tiebreaker prescient-tiebreaker))
    (prescient--sort-compare)))

(defvar prescient-query nil
  "The original query from the user, if available.
You can use this in your implementation of `prescient-tiebreaker'
to sort candidates depending on the user's query. This might be
nil if `prescient-sort-compare' is invoked directly, or if
`prescient-sort' is invoked without `prescient-filter' having
been run first, so you should handle that case too.")

(defun prescient-sort (candidates)
  "Sort CANDIDATES using frequency data.
Return the sorted list. The original is modified destructively.

See also the functions `prescient-sort-full-matches-first' and
`prescient-completion-sort'. Both are meant to be used after
`prescient-filter'."
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
        (len-enable prescient-sort-length-enable)
        (tiebreaker prescient-tiebreaker)
        (prescient-query (plist-get (prescient--get-sort-info candidates)
                                    :prescient-query)))
    (sort
     candidates
     (lambda (c1 c2)
       (prescient--sort-compare)))))

(defun prescient-sort-full-matches-first (candidates regexps ignore-case)
  "Sort fully matched strings in CANDIDATES before other candidates.

REGEXPS are the regexps prescient.el used to filter the candidates.
IGNORE-CASE is whether case was ignored when filtering.

As this function is meant to be used after filtering, all of the
candidates in CANDIDATES should match all of the regexps in
REGEXPS."
  (cond
   ((null candidates) nil)
   ((null regexps) candidates)
   (t (save-match-data
        (cl-loop
         with prioritized-candidates = nil
         and remaining-candidates = nil
         and case-fold-search = ignore-case
         for cand in candidates
         if (cl-loop for regexp in regexps
                     thereis (and (string-match regexp cand)
                                  (= (length cand)
                                     (- (match-end 0)
                                        (match-beginning 0)))))
         do (push cand prioritized-candidates)
         else do (push cand remaining-candidates)
         finally return (nconc (nreverse prioritized-candidates)
                               (nreverse remaining-candidates)))))))

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

;;;; Completion Style

;; This section contains functions for implementing the `prescient'
;; completion style. This feature is based on Orderless.el.
;; See: https://github.com/oantolin/orderless

(defvar completion-lazy-hilit)
(defvar completion-lazy-hilit-fn)

;;;;; Sorting functions

;;;###autoload
(cl-defun prescient-completion-sort (candidates)
  "Sort the filtered CANDIDATES.

This function will always sort candidates using the function
`prescient-sort'. When CANDIDATES has been filtered using the
`prescient' completion style, it can optionally also sort them
using the function `prescient-sort-full-matches-first'.

This function checks for the properties `prescient-regexps' and
`prescient-ignore-case' on any candidate in CANDIDATES (though
they are stored on the first candidate returned by
`prescient-filter'). These properties are used for implementing
the user option `prescient-sort-full-matches-first'."
  (if (null candidates)
      nil
    ;; `prescient-filter' adds the properties needed for
    ;; `prescient-sort-full-matches-first' to the first candidate in
    ;; the list it returns. If we're receiving the filtered candidates
    ;; directly (so, not in `company-prescient-transformer') then we
    ;; should be checking for them before running `prescient-sort',
    ;; which destructively modifies CANDIDATES.
    (let ((regexps)
          (ignore-case))
      (when prescient-sort-full-matches-first
        (let ((props (prescient--get-sort-info candidates)))
          (setq regexps (plist-get props :prescient-all-regexps)
                ignore-case (plist-get props :prescient-ignore-case))))
      (thread-first
        candidates
        (prescient-sort)
        ;; If `regexps' is nil, this just returns the input.
        (prescient-sort-full-matches-first regexps ignore-case)))))

;;;;; Filtering functions

;;;###autoload
(defun prescient-all-completions (string table &optional pred _point)
  "`all-completions' using prescient.el.

STRING is the input. TABLE is a completion table. PRED is a
predicate that further restricts the matching candidates. POINT
would be the current point, but it is not used by this function.
See the function `all-completions' for more information.

This function returns a list of completions whose final `cdr' is
the length of the prefix string used for completion (which might
be all or just part of STRING).

When `completion-lazy-hilit' is bound and non-nil, then this
function sets `completion-lazy-hilit-fn'. Otherwise, if
`prescient-completion-highlight-matches' is non-nil, this
function propertizes all of the returned completions using the
face `prescient-primary-highlight' and the face
`prescient-secondary-highlight'."
  ;; `point' is a required argument, but unneeded here.
  (when-let ((completions (prescient-filter string table pred)))
    (pcase-let* ((`(,prefix . ,pattern)
                  (prescient--prefix-and-pattern string table pred))
                 (maybe-highlighted
                  (cond
                   ((bound-and-true-p completion-lazy-hilit)
                    (setq completion-lazy-hilit-fn
                          (apply-partially
                           #'prescient--highlight-candidate
                           (prescient-filter-regexps pattern 'with-group)
                           (prescient-ignore-case-p pattern)))
                    completions)
                   (prescient-completion-highlight-matches
                    (prescient--highlight-candidates pattern completions))
                   (t
                    completions))))
      (nconc maybe-highlighted (length prefix)))))

;;;###autoload
(defun prescient-try-completion (string table &optional pred point)
  "`try-completion' using Prescient.

STRING is the input.  TABLE is a completion table.  PRED is a
predicate.  POINT is the current point.  See the function
`try-completion' for more information.

If there are no matches, this function returns nil. If the only
match equals STRING, this function returns t. Otherwise, this
function returns a cons cell of the completed string and its
length. If there is more than one match, that completed string is
actually just the input, in which case nothing happens."
  (when-let ((completions (prescient-filter string table pred)))
    (if (cdr completions)
        (cons string point) ; Multiple matches
      (let ((match (car completions)))
        (if (equal string match)
            t ; Literal input equals only match.
          ;; Otherwise, return the match and move point to its end.
          (let* ((prefix (car (prescient--prefix-and-pattern
                               string table pred)))
                 (full (concat prefix match)))
            (cons full (length full))))))))

;;;;; Setting up the completion style

;;;###autoload
(add-to-list
 'completion-styles-alist
 '( prescient prescient-try-completion prescient-all-completions
    "Filtering using prescient.el.
For sorting, see the function `prescient-completion-sort'."))

;;;;; Component functions for completion-style minor modes

;; These functions are used to implement the integration packages
;; for Corfu and Vertico, which use completion styles.

(defconst prescient--completion-recommended-styles '(prescient basic)
  "Recommended completions styles for using `prescient'.")

(defconst prescient--completion-recommended-overrides
  '(;; Include `partial-completion' to enable wildcards and
    ;; partial paths.
    (file (styles basic partial-completion))
    ;; Eglot forces `flex' by default.
    (eglot (styles prescient basic)))
  "Recommended completion-category overrides for using prescient.")

(defconst prescient--completion-settings-vars
  '( completion-styles completion-category-overrides
     completion-category-defaults)
  "Variables that are changed to configure filtering.")

(defvar prescient--completion-old-styles nil
  "Previous value of `completion-styles'.")

(defvar prescient--completion-old-overrides nil
  "Previous value of `completion-category-overrides'.")

(defvar prescient--completion-old-defaults nil
  "Previous value of `completion-category-defaults'.")

(defconst prescient--completion-old-vars
  '( prescient--completion-old-styles
     prescient--completion-old-overrides
     prescient--completion-old-defaults)
  "Variables used to store old settings.")

(cl-defun prescient--completion-apply-completion-settings
    (&key (styles prescient--completion-recommended-styles)
          (overrides prescient--completion-recommended-overrides))
  "Modify the user options and variables.

STYLES is the new `completion-styles'. OVERRIDES is new
overrides for `completion-category-overrides'.
`completion-category-defaults' is set to nil. These variables
are listed in `prescient--completion-settings-vars'.

While there are recommended settings, these can be overridden by
user options in the extension packages."
  (setq completion-styles styles
        completion-category-defaults nil)

  (cl-symbol-macrolet
      ((category-setting-overrides
        (alist-get setting
                   (alist-get category completion-category-overrides))))
    (cl-loop for (category . overrides)
             in overrides
             do (cl-loop for (setting . values) in overrides
                         do (setf category-setting-overrides values)))))

(defun prescient--completion-save-completion-settings ()
  "Save the old completion filtering settings.

Values are saved in `prescient--completion-old-styles',
`prescient--completion-old-defaults', and
`prescient--completion-old-overrides', which are listed in
`prescient--completion-old-vars'."
  (setq prescient--completion-old-styles completion-styles
        prescient--completion-old-defaults completion-category-defaults)

  (cl-symbol-macrolet
      ((category-setting-overrides
        (alist-get setting
                   (alist-get category completion-category-overrides))))
    (setq prescient--completion-old-overrides
          (cl-loop
           for (category . overrides)
           in prescient--completion-recommended-overrides
           collect
           `(,category ,@(cl-loop
                          for (setting . _) in overrides
                          collect
                          `(,setting ,@category-setting-overrides)))))))

(cl-defun prescient--completion-restore-completion-settings
    (&key (styles prescient--completion-recommended-styles)
          (overrides prescient--completion-recommended-overrides))
  "Restore the old settings.

STYLES is what `completion-styles' was changed to. OVERRIDES are
the overrides that were changed in `completion-category-overrides'.

If the current values of the settings variables do not match the
changes made by `prescient--completion-apply-completion-settings',
then we don't restore the previous values and instead only try to
remove usages of the `prescient' completion style."
  ;; Try to revert back to old settings, or at least not use the
  ;; `prescient' style.
  (if (equal completion-styles styles)
      (setq completion-styles prescient--completion-old-styles)
    (cl-callf2 remq 'prescient completion-styles))

  (cl-loop for (key . val) in prescient--completion-old-defaults
           unless (alist-get key completion-category-defaults)
           do (setf (alist-get key completion-category-defaults) val))

  (cl-symbol-macrolet
      ((category-setting-overrides
        (alist-get
         setting
         (alist-get category completion-category-overrides))))
    (cl-loop
     ;; These two trees should have the same structure by this
     ;; point. We want to try to avoid undoing any changes that were
     ;; made after the mode was enabled.
     for (category . new-overrides)
     in overrides
     for (_ . old-overrides)
     in prescient--completion-old-overrides
     do (cl-loop
         for (setting . new-values) in new-overrides
         for (_ . old-values) in old-overrides
         if (equal category-setting-overrides new-values)
         do (setf category-setting-overrides old-values)
         else do (cl-callf2 remq 'prescient category-setting-overrides)))))

(defvar-local prescient--completion-vars-already-local nil
  "Variables of interest that were already buffer local.

Extension packages might wish to configure the variables listed
in `prescient--completion-settings-vars' buffer locally, whose
localness should be undone when the extension mode is disabled.
This list is to prevent extension modes from killing variables
that were already local when the mode was enabled.")

(defun prescient--completion-make-vars-local ()
  "Make the settings and restoration variables buffer local.

Record whether a settings variable (for example,
`completion-styles') was already local in
`prescient--completion-vars-already-local'.

This must happen before storing old values."
  (mapc #'make-local-variable prescient--completion-old-vars)
  (dolist (var prescient--completion-settings-vars)
    (if (local-variable-p var)
        (push var prescient--completion-vars-already-local)
      (make-local-variable var))))

(defun prescient--completion-kill-local-vars ()
  "Kill local settings and restoration variables.

Don't kill the variables if they are members of
`prescient--completion-vars-already-local'.

This must happen after restoring old values."
  (dolist (var prescient--completion-settings-vars)
    (unless (memq var prescient--completion-vars-already-local)
      (kill-local-variable var)))
  (setq prescient--completion-vars-already-local nil))

;;;; Toggling commands
;; These commands are meant to be bound in a completion UI, in which
;; `prescient-filter-method' can be bound buffer locally.

(defconst prescient--toggle-vars
  '( prescient-filter-method prescient-use-case-folding
     prescient-use-char-folding)
  "Variables that can be changed locally by toggling commands.

An explicit list is needed for Corfu.")

(defvar prescient--toggle-refresh-functions nil
  "Functions to run to force refreshing the completion UI.
Functions are added by the integration packages.")

(defun prescient--toggle-refresh ()
  "Run the UI refresh functions."
  (run-hooks 'prescient--toggle-refresh-functions))

(defvar prescient-toggle-map (make-sparse-keymap)
  "Toggling commands for `prescient.el' filters in minibuffer completion.
This map is automatically bound by the integration packages.")

;;;###autoload
(defmacro prescient-create-and-bind-toggle-command
    (filter-type kbd-string)
  "Create and bind a command to toggle the use of a filter method.

The created command toggles the FILTER-TYPE method on
or off buffer locally, and doesn't affect the default
behavior (determined by `prescient-filter-method').

The created command is bound to KBD-STRING in
`prescient-toggle-map'. This map is itself bound to `M-s'
in the completion buffer when `selectrum-prescient-mode' or
`vertico-prescient-mode' are enabled.

FILTER-TYPE is an unquoted symbol that can be used in
`prescient-filter-method'. KBD-STRING is a string that can be
passed to `kbd'."
  (let* ((filter-type-name (symbol-name filter-type)))
    `(define-key prescient-toggle-map (kbd ,kbd-string)
       (defun ,(intern (concat "prescient-toggle-" filter-type-name))
           (arg)                    ; Arg list
         ,(format
           "Toggle the \"%s\" filter on or off. With ARG, use only this filter.
This toggling only affects filtering in the current completion
buffer. It does not affect the default behavior (determined by
`prescient-filter-method')."  filter-type-name)
         (interactive "P")

         ;; Make `prescient-filter-method' buffer-local in the
         ;; completion buffer. We don't want to accidentally change the
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
                ,(concat
                  "Prescient.el: Can't toggle off only active filter method: "
                  filter-type-name))

             (setq prescient-filter-method
                   (if (memq ',filter-type prescient-filter-method)
                       ;; Even when running `make-local-variable',
                       ;; it seems `delq' might still modify the
                       ;; global value, so we use `remq' here.
                       (remq ',filter-type prescient-filter-method)
                     (cons ',filter-type prescient-filter-method)))))

         ;; After changing `prescient-filter-method', tell the user
         ;; the new value and update the UI's display.
         (message "Prescient.el filter is now %s"
                  prescient-filter-method)

         ;; Call "exhibit" function.
         (prescient--toggle-refresh)))))

(prescient-create-and-bind-toggle-command anchored "a")
(prescient-create-and-bind-toggle-command fuzzy "f")
(prescient-create-and-bind-toggle-command initialism "i")
(prescient-create-and-bind-toggle-command literal "l")
(prescient-create-and-bind-toggle-command literal-prefix "P")
(prescient-create-and-bind-toggle-command prefix "p")
(prescient-create-and-bind-toggle-command regexp "r")

(defun prescient-toggle-char-fold ()
  "Toggle character folding in the current completion buffer.

See the user option `prescient-use-char-folding'."
  (interactive)
  (setq-local prescient-use-char-folding
              (not prescient-use-char-folding))
  (message "Character folding toggled %s"
           (if prescient-use-char-folding "on" "off"))
  (prescient--toggle-refresh))

;; This is the same binding used by `isearch-toggle-char-fold'.
(define-key prescient-toggle-map (kbd "'")
  #'prescient-toggle-char-fold)

(defun prescient-toggle-case-fold ()
  "Toggle case folding in the current completion buffer.

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
  (prescient--toggle-refresh))

;; This is the same binding used by `isearch-toggle-case-fold'.
(define-key prescient-toggle-map (kbd "c")
  #'prescient-toggle-case-fold)


;;;; Closing remarks
(provide 'prescient)

;;; prescient.el ends here
