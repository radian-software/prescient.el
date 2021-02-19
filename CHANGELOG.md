# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
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

    | Key     | Command                                 |
    |---------|-----------------------------------------|
    | `M-s a` | `selectrum-prescient-toggle-anchored`   |
    | `M-s f` | `selectrum-prescient-toggle-fuzzy`      |
    | `M-s i` | `selectrum-prescient-toggle-initialism` |
    | `M-s l` | `selectrum-prescient-toggle-literal`    |
    | `M-s p` | `selectrum-prescient-toggle-prefix`     |
    | `M-s r` | `selectrum-prescient-toggle-regexp`     |

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

* The user option `prescient-sort-full-matches-first` was added. If
  non-nil, candidates that are fully matched are sorted before
  partially matched candidates, though all candidates still follow the
  order of recency, frequency, and length. See [#95].

[#66]: https://github.com/raxod502/prescient.el/pull/66
[#67]: https://github.com/raxod502/prescient.el/pull/67
[#70]: https://github.com/raxod502/prescient.el/pull/70
[#72]: https://github.com/raxod502/prescient.el/pull/72
[#76]: https://github.com/raxod502/prescient.el/pull/76
[#77]: https://github.com/raxod502/prescient.el/pull/77
[#95]: https://github.com/raxod502/prescient.el/pull/95

## 5.0 (release 2020-07-16)
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
