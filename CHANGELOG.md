# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
### New features
* Add package `vertico-prescient`, which integrates prescient.el with
  Vertico ([#131]). New mode `vertico-prescient-mode` configures
  sorting, candidate remembrance, filtering, and binds the toggling
  commands in Vertico.

* Add package `corfu-prescient`, which integrates prescient.el with
  Corfu ([#131]). New mode `corfu-prescient-mode` configures
  sorting, candidate remembrance, filtering, and binds the toggling
  commands while the Corfu pop-up is active.

## 6.0 (released 2022-11-11)
### Bugs fixed
* Toggling off filter methods no longer accidentally changes the
  global value of `prescient-filter-method`.  See [#123], [#124].

* Always `require` the library `char-fold` so that `char-fold-table`
  is defined. This variable apparently isn't always loaded when we
  call `char-fold-to-regexp`. See [#126], [#127].

* Fix the filter methods `literal` and `literal-prefix` not being
  literal when `prescient-use-char-folding` was nil. This bug was
  added with that user option. See [#127].

### New features
* Add new function `prescient-sort-full-matches-first` which
  implements the option. This feature already existed, but moving to a
  separate function makes it easier to support in more UIs. See
  [#125].

* Add a completion style `prescient`. This completion style can be
  used in the variable `completion-styles`. This completion style
  works with UIs like Emacs's built-in minibuffer completion,
  Icomplete, and Vertico. See various discussions in [#125], [#120],
  [#112], [#89], [#58], and [#54].

* Add new function `prescient-completion-sort`, which combines
  `prescient-sort` with the new function
  `prescient-sort-full-matches-first` ([#125]). This function is
  meant to be used after filtering and as the sorting function of your
  preferred completion UI. Note that sorting fully matched candidates
  before partially matched candidates only works for candidates
  filtered by the `prescient` completion style.

* Added user option `prescient-completion-highlight-matches`, which
  determines whether the completion style highlights the matching
  parts of candidates with the above new faces ([#125]).

* Add faces `prescient-primary-highlight` and
  `prescient-secondary-highlight` ([#125]). These faces are used with
  the completion style and `selectrum-prescient.el`. The old faces
  `selectrum-prescient-primary-highlight` and
  `selectrum-prescient-secondary-highlight` are now obsolete aliases
  of these faces.

### Enhancements
* `prescient-filter` now supports filtering candidates from Emacs's
  more generic "completion tables", not just lists of strings ([#125]).
  However, like with some other completion styles, it does not work
  well with certain dynamic completion tables that use a prefix string
  to produce candidates before filtering. To work around this, it is
  recommended to include the `basic` style after the `prescient` style
  in the user option `completion-styles`.

### Internal Changes
* `prescient-filter` now uses the C function `all-completions` instead
  of being completely written in Emacs Lisp. This should make it a bit
  faster. See [#125].

[#54]: https://github.com/raxod502/prescient.el/issues/54
[#58]: https://github.com/raxod502/prescient.el/issues/58
[#89]: https://github.com/raxod502/prescient.el/issues/89
[#112]: https://github.com/raxod502/prescient.el/issues/112
[#120]: https://github.com/raxod502/prescient.el/issues/120
[#123]: https://github.com/radian-software/prescient.el/issues/123
[#124]: https://github.com/radian-software/prescient.el/pull/124
[#125]: https://github.com/raxod502/prescient.el/pull/125
[#126]: https://github.com/radian-software/prescient.el/pull/126
[#127]: https://github.com/radian-software/prescient.el/pull/127
[#131]: https://github.com/radian-software/prescient.el/pull/131

## 5.2.1 (released 2022-06-01)
### Bugs fixed
* ivy doesn't convert all variables to string when sorting or calling
  `prescient-remember`, so it need to preprocess when work with
  `ivy-prescient.el`. if not, an `wrong-type-argument` error may
  occur. This issue affects the use of `format-all-buffer` ([#119]).

[#119]: https://github.com/raxod502/prescient.el/pull/119

## 5.2 (released 2021-12-27)
### New features
* Two new user options, `selectrum-prescient-enable-filtering` and
  `selectrum-prescient-enable-sorting`, which allow the user to
  selectively disable the filtering or sorting functionalities of
  `selectrum-prescient.el` ([#100]).
* New command `selectrum-prescient-toggle-char-fold`, bound to `M-s '`
  in the minibuffer.
* To configure match highlighting you can use the faces
  `selectrum-prescient-primary-highlight` and
  `selectrum-prescient-secondary-highlight`. The previously used
  Selectrum faces `selectrum-primary-highlight` and
  `selectrum-secondary-highlight` will get removed from Selectrum
  proper ([#94], [#97]).
* The user option `prescient-sort-full-matches-first` was added. If
  non-nil, candidates that are fully matched are sorted before
  partially matched candidates, though all candidates still follow the
  order of recency, frequency, and length. See [#95].
* The user option `prescient-use-char-folding` was added. If non-nil,
  the `literal` and `literal-prefix` filter methods will use character
  folding. See [#98]. This can be used to help avoid the problems
  reported in [#92] and [#93].
* The user option `prescient-use-case-folding` was added.  This
  feature affects the use of all filters.  It can be one of `nil`,
  `t`, or `smart` (the default).  If `smart`, then case folding is
  disabled when upper-case characters are sought.  In Selectrum, the
  toggling command `selectrum-prescient-toggle-case-fold` was bound to
  `M-s c`.  See [#105].
* The command `prescient-forget` was added. When used, `prescient.el`
  will immediately forget a candidate ([#109]).

### Enhancements
* `prescient-filter-method` accepts a function which returns the
  desired filter methods ([#110]).
* `selectrum-prescient.el`: Match faces are now combined with faces
  that might be already present on candidates instead of replacing
  them which gives better visual results in these cases ([#101],
  [#103]).

### Internal changes
* The user option `selectrum-should-sort-p` was deprecated in
  Selectrum 3.1 in favor of `selectrum-should-sort`.
  `selectrum-prescient.el` now uses the updated option name ([#99]).

### Bugs fixed
* A typo was fixed that prevented secondary highlighting (such as the
  initials in `initialism` matching) from being applied.  Functions in
  `prescient-filter-alist` were being passed the keyword argument
  `:with-groups` instead of the correct `:with-group`.  For
  consistency, the `with-groups` argument of
  `prescient-filter-regexps` was changed to `with-group`.  See [#106].
* Previously, when char folding was enabled, long queries could cause
  a crash with the error "Regular expression too big". This has now
  been fixed ([#71]).

[#71]: https://github.com/raxod502/prescient.el/issues/71
[#92]: https://github.com/raxod502/prescient.el/issues/92
[#93]: https://github.com/raxod503/prescient.el/issues/93
[#94]: https://github.com/raxod502/prescient.el/pull/94
[#95]: https://github.com/raxod502/prescient.el/pull/95
[#97]: https://github.com/raxod502/prescient.el/pull/97
[#98]: https://github.com/raxod502/prescient.el/pull/98
[#99]: https://github.com/raxod502/prescient.el/pull/99
[#100]: https://github.com/raxod502/prescient.el/pull/100
[#101]: https://github.com/raxod503/prescient.el/issues/101
[#103]: https://github.com/raxod502/prescient.el/pull/103
[#105]: https://github.com/raxod502/prescient.el/pull/105
[#106]: https://github.com/raxod502/prescient.el/pull/106
[#109]: https://github.com/raxod502/prescient.el/pull/109
[#110]: https://github.com/raxod502/prescient.el/pull/110

## 5.1 (released 2021-02-26)
### Enhancements
* Literal-prefix matching, a new filter method whose behavior is that
  the first subquery must be the prefix of the candidate and the
  remaining subqueries must be prefixes of words in the
  candidate. Supports char folding just like `literal`.

  For example, if the input is `foo bar`, then the candidate must
  begin with `foo`, and it must also contain a word starting with
  `bar`.  That means it would match `foo-and-bar` or `fooboo-barquux`
  but not `bar-foo` (because that doesn't start with `foo`) or
  `foo-qbar` (because `bar` is no prefix of some word).

  It can be enabled by adding `literal-prefix` to
  `prescient-filter-method`.
* Anchored matching, a new filtering method that uses uppercase
  letters and symbols as beginning of word, similar to initialism.
  It can be enabled by adding `anchored` to
  `prescient-filter-method`.

  For example `TTL` matches `toogle-truncate-lines` and `FiAPo`
  or `fiAPo` match both `find-file-at-point` and
  `find-function-at-point`. However `fiFiAPo` matches only the former
  and `fiFuAPo` matches only the latter. See [#70] and [#76].
* Prefix matching, a new filtering method similar to the Emacs
  completion style `partial`, was added. It can be enabled by adding
  `prefix` to `prescient-filter-method`.

  As is the case in partial completion, `t-t-l` matches
  `toggle-truncate-lines` and `fi--a-po` matches `find-file-at-point`,
  `find-function-at-point`, and other similarly named symbols. One
  difference is that you can't use `*` as a wildcard (it is instead
  taken literally), since you can achieve the same effect by
  separating queries with a space. See [#67] and [#76].
* Literal matching now supports char folding making Unicode text
  filtering much easier ([#66]).
* In `selectrum-prescient.el`, commands were added for toggling the
  active filtering methods in the Selectrum buffer. See [#72].
  * This toggling is a buffer-local effect, and doesn't change the
    default behavior (determined by `prescient-filter-method`).
  * With a prefix argument, a command unconditionally toggles on its
    respective filtering method and toggles off all others.
  * While `selectrum-prescient-mode` is enabled, `M-s` is bound to
    `selectrum-prescient-toggle-map` in the Selectrum buffer, and is
    used as a prefix key to access the commands. The macro
    `selectrum-prescient-create-and-bind-toggle-command` can be used
    to create a toggling command for a filter, and bind that command
    in `selectrum-prescient-toggle-map`.

    | Key     | Command                                     |
    |---------|---------------------------------------------|
    | `M-s a` | `selectrum-prescient-toggle-anchored`       |
    | `M-s f` | `selectrum-prescient-toggle-fuzzy`          |
    | `M-s i` | `selectrum-prescient-toggle-initialism`     |
    | `M-s l` | `selectrum-prescient-toggle-literal`        |
    | `M-s p` | `selectrum-prescient-toggle-prefix`         |
    | `M-s P` | `selectrum-prescient-toggle-literal-prefix` |
    | `M-s r` | `selectrum-prescient-toggle-regexp`         |
    | `M-s '` |       |

* The user option `prescient-filter-alist` was added, which
  describes the relationship between the symbols in
  `prescient-filter-list` and the corresponding functions that produce
  regular expressions for matching candidates. Users can create their
  own filter methods by adding a symbol-function pair to
  `prescient-filter-alist` and use that custom method by adding the
  symbol to `prescient-filter-method`. See [#77].

  The functions that produce the regexps used for searching candidates
  are now considered public. They are
    - `prescient-anchored-regexp`
    - `prescient-fuzzy-regexp`
    - `prescient-initials-regexp`
    - `prescient-literal-regexp`
    - `prescient-prefix-regexp`
    - `prescient-regexp-regexp`
    - `prescient-with-group`

[#66]: https://github.com/raxod502/prescient.el/pull/66
[#67]: https://github.com/raxod502/prescient.el/pull/67
[#70]: https://github.com/raxod502/prescient.el/pull/70
[#72]: https://github.com/raxod502/prescient.el/pull/72
[#76]: https://github.com/raxod502/prescient.el/pull/76
[#77]: https://github.com/raxod502/prescient.el/pull/77

## 5.0 (released 2020-07-16)
### Breaking changes
* Candidates which are not strings are no longer supported. This
  improves performance by a few percent, and there was never any real
  reason to support non-strings. (Frameworks like Selectrum should
  convert candidates to strings before interfacing with
  `prescient.el`.)

### Enhancements
* Fuzzy matching now uses lazy wildcards, so only the minimum amount
  of each candidate will be highlighted in results from Selectrum or
  Ivy, rather than the maximum.

### Bugs fixed
* In `selectrum-prescient.el`, secondary highlighting now works
  correctly when multiple filter methods are enabled ([#123]).

### Performance
* The speed of `prescient-sort` has been improved by a factor of 2x
  for large collections. The speed of `prescient-filter` has been
  improved by a factor of 3x.

[#123]: https://github.com/raxod502/selectrum/issues/123

## 4.1 (released 2020-03-31)
### Enhancements
* Package `selectrum-prescient.el` now uses
  `selectrum-candidate-inserted-hook` to update the frecency database
  of `prescient.el` when using `TAB`
  (`selectrum-insert-current-candidate`) to enter directories while
  finding files with Selectrum.

### Bugs fixed
* Previously, if you explicitly enabled any of the minor modes
  `ivy-prescient-mode`, `company-prescient-mode`, or
  `selectrum-prescient-mode` when it was already on, then disabling
  the mode would not work correctly (some variables would fail to be
  reset). This has been fixed.

## 4.0 (released 2019-12-21)
### Breaking changes
* Due to internal changes in Ivy and Counsel which make it extremely
  difficult and brittle to control sorting in a fine-grained manner,
  candidates for all commands are now sorted by default. You can still
  configure the behavior in `ivy-prescient-sort-commands`, which has a
  new default value and now also has the ability to specify a
  *blacklist* of commands (used in the new default, which excludes
  `swiper` and `ivy-switch-buffer`) in addition to a whitelist (used
  in the old default).

### Added
* New package `selectrum-prescient` provides integration with
  [Selectrum], a modern alternative to Ivy and Helm. It provides a
  single entry point, `selectrum-prescient-mode`, which may be enabled
  in your init-file. This package is likely to be essentially bug-free
  when compared to `ivy-prescient`, because of the vastly simpler API
  on which it depends.

### Enhancements
* The function `prescient-filter` usually made a new copy of the input
  list, but didn't do this in the case that there was nothing to
  filter out. That was subtle behavior with the potential for causing
  bugs, so now `prescient-filter` is guaranteed to always make a copy
  of the input list.

### Bugs fixed
* Recent commits to Counsel (including [`9da800`][9da800] and
  [`2d840b`][2d840b]) resulted in candidates for many commands no
  longer being sorted. This has been fixed.

### Removed
* Function `prescient-initials-regexp`, which was deprecated in
  release 2.2 over a year ago.

[selectrum]: https://github.com/raxod502/selectrum

[2d840b]: https://github.com/abo-abo/swiper/commit/2d840b8be54fbc887a6b2a6a8178d27786d1421f
[9da800]: https://github.com/abo-abo/swiper/commit/9da800306a82e85ac6d62d0b86baa731b394807e

## 3.3 (released 2019-09-20)
### Enhancements
* New user option `prescient-aggressive-file-save` to enable
  saving the cache data more aggressively according to the
  discussions ([#17] and [#39]).
* The minor mode `ivy-prescient-mode` now affects the
  `completion-in-region` command by default, without the need for the
  user to change the value of `ivy-sort-functions-alist` ([#41]).

### Bugs fixed
* An update to Ivy caused `ivy-prescient.el` to no longer have any
  effect for the `ivy-completion-in-region` command. This has been
  fixed ([#41], [#42]).

[#17]: https://github.com/raxod502/prescient.el/issues/17
[#39]: https://github.com/raxod502/prescient.el/pull/39
[#41]: https://github.com/raxod502/prescient.el/issues/41
[#42]: https://github.com/raxod502/prescient.el/pull/42

## 3.2 (released 2019-07-06)
### New features
* New user options `prescient-sort-length-enable` and
  `company-prescient-sort-length-enable` allow disabling
  `prescient.el` sorting by length, without disabling sorting
  altogether.

### Enhancements
* The `fuzzy` regex builder now matches the first occurrence of each
  character in the query, giving more intuitive results when
  highlighted ([#37]).

### Bugs fixed
* Logic error which caused a crash on an all-whitespace query has now
  been fixed.

[#37]: https://github.com/raxod502/prescient.el/pull/37

## 3.1 (released 2019-05-23)
### Enhancements
* Previously, `ivy-prescient.el` would only remember directories that
  the user operated on with an action (for example, opening them with
  Dired). Now it will remember all intermediate directories used in
  navigation ([#36]).

[#36]: https://github.com/raxod502/prescient.el/pull/36

## 3.0 (released 2019-04-10)
### New features
* The user option `prescient-filter-method` now accepts a list of
  filter methods that will be applied in order until one matches. This
  allows for any combination of filter methods ([#29]). The default
  value has changed from `literal+initialism` to `(literal regexp
  initialism)`, which provides a superset of functionality.
* Two new user options, `ivy-prescient-enable-filtering` and
  `ivy-prescient-enable-sorting`, which allow the user to selectively
  disable the filtering or sorting functionalities of
  `ivy-prescient.el` ([#32]).

### Enhancements
* Previously, if you had set `prescient-filter-method` to `regexp` and
  entered an invalid regexp with `ivy-prescient.el`, then the
  candidate list would not update. Now, the list is cleared until your
  regexp is valid again. This behavior is more consistent and
  predictable.

### Bugs fixed
* Fixed sorting in filename-based collections ([#28]).
* Fixed an issue where Ivy actions that accepted a list of candidates
  were being handled incorrectly ([#33]).

### Removed
* Removed `ivy-prescient-filter-method-keys` and
  `ivy-prescient-persist-filter-method` because `regexp` is now part
  of the default filter method, and therefore these are no longer
  needed ([#15], [#24], and [#27]).
* Removed `ivy-prescient-excluded-commands` because `ivy-prescient.el`
  now automatically detects if sorting is enabled in a collection.

[#15]: https://github.com/raxod502/prescient.el/issues/15
[#24]: https://github.com/raxod502/prescient.el/issues/24
[#27]: https://github.com/raxod502/prescient.el/issues/27
[#28]: https://github.com/raxod502/prescient.el/issues/28
[#29]: https://github.com/raxod502/prescient.el/issues/29
[#32]: https://github.com/raxod502/prescient.el/issues/32
[#33]: https://github.com/raxod502/prescient.el/issues/33

## 2.2.2 (released 2019-02-12)
### Enhancements
* The Custom groups for the user options `prescient-persist-mode`,
  `ivy-prescient-mode`, and `company-prescient-mode` have now been set
  properly to `prescient` (instead of each being in its own group).

### Bugs fixed
* `ivy-prescient` now depends on version 0.11.0 of `ivy`, rather than
  0.10.0. This [long-awaited] release includes a number of bugfixes
  which are critical for correct operation of `ivy-prescient`.

[long-awaited]: https://github.com/abo-abo/swiper/issues/1664

## 2.2.1 (released 2018-09-22)
### Fixed
* Previously, if the user or some other code had customized
  `print-length` or `print-level`, then these settings could break the
  ability of `prescient.el` to persist history and frequency data to
  `prescient-save-file`. Now, persistence works regardless of the
  values of these variables ([#16]).

[#16]: https://github.com/raxod502/prescient.el/issues/16

## 2.2 (released 2018-08-23)
### New features
* New user option `prescient-filter-method` allows you to use
  substring, initialism, regexp, or fuzzy matching ([#13]).
* By default, you can use `C-c C-r` in Ivy to toggle between
  substring/initialism matching and regexp matching. Bindings can be
  customized in the new user option
  `ivy-prescient-filter-method-keys`. By default, the changes do not
  persist between Ivy sessions, but this can be customized using the
  new user option `ivy-prescient-persist-filter-method`.

### Deprecated
* The function `prescient-initials-regexp` is now considered
  deprecated and it should not be used. Changes to its behavior will
  however be documented until it is removed in a future release.

[#13]: https://github.com/raxod502/prescient.el/issues/13

## 2.1 (released 2018-07-25)
### New features
* New user option `ivy-prescient-retain-classic-highlighting` allows
  you to more closely emulate the highlighting behavior of Ivy when
  `ivy-prescient-mode` is enabled ([#11]).

### Enhancements
* API function `prescient-filter-regexps` now has special behavior
  when the symbol `all` is passed as the `WITH-GROUPS` argument. The
  documentation string for this function, which was previously
  erroneous, has been fixed ([#11]).
* For some reason, Ivy explicitly supports candidates which are lists,
  by just taking their cars. `ivy-prescient.el` now does the same,
  instead of converting the lists to strings. This reduces bloat in
  the save file, and in some cases improves frequency tracking (for
  example, frequency tracking works much better for
  `org-refile-get-location`).

[#11]: https://github.com/raxod502/prescient.el/issues/11

## 2.0 (released 2018-06-26)
### Changed
* `ivy-prescient.el` now clears the value of
  `ivy-initial-inputs-alist` when `ivy-prescient-mode` is enabled, in
  order to allow functions like `counsel-describe-function` to work
  out of the box ([#7]).

### Fixed
* Previously, `ivy-prescient.el` didn't work with the latest version
  of `counsel-projectile` because that package relied on certain
  undocumented semantics of the `ivy-read` API. This is now fixed
  ([#6]).

[#6]: https://github.com/raxod502/prescient.el/issues/6
[#7]: https://github.com/raxod502/prescient.el/issues/7

## 1.0 (released 2018-06-05)
### Added
* Packages:
  * `prescient.el`
  * `ivy-prescient.el`
  * `company-prescient.el`
* User options:
  * `prescient-history-length`
  * `prescient-frequency-decay`
  * `prescient-frequency-threshold`
  * `prescient-save-file`
  * `ivy-prescient-excluded-commands`
  * `ivy-prescient-sort-commands`
* Minor modes:
  * `prescient-persist-mode`
  * `ivy-prescient-mode`
  * `company-prescient-mode`
* API functions:
  * `prescient-split-query`
  * `prescient-initials-regexp`
  * `prescient-filter-regexps`
  * `prescient-filter`
  * `prescient-sort-compare`
  * `prescient-sort`
  * `prescient-remember`

[keep a changelog]: https://keepachangelog.com/en/1.0.0/
