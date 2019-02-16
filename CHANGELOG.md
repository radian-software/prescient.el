# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
### New features
* The user option `prescient-filter-method` now accepts a list of
  filter methods that will be applied in order until one matches. This
  changes the new default changed from `literal+initialism` to
  `(literal initialism)`, which is functionally equivalent. This
  allows for any combination of filter methods ([#29]).

[#29]: https://github.com/raxod502/prescient.el/issues/29

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
