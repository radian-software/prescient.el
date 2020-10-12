**prescient.el:** simple but effective sorting and filtering for
Emacs.

## Summary

`prescient.el` is a library which sorts and filters lists of
candidates, such as appear when you use a package like [Ivy] or
[Company]. Extension packages such as `ivy-prescient.el` and
`company-prescient.el` adapt the library for usage with various
frameworks.

As compared to other packages which accomplish similar tasks,
including [IDO], [Ivy], [Helm], [Smex], [Flx], [Historian], and
[Company-Statistics], `prescient.el` aims to be simpler, more
predictable, and faster.

## Installation

`prescient.el` is available on MELPA as four separate packages (one
for the library, and the rest for integrating with other frameworks):

* [`prescient`](https://melpa.org/#/prescient)
* [`ivy-prescient`](https://melpa.org/#/ivy-prescient)
* [`company-prescient`](https://melpa.org/#/company-prescient)
* [`selectrum-prescient`](https://melpa.org/#/selectrum-prescient)

The easiest way to install these packages is using
[`straight.el`][straight.el]:

    (straight-use-package 'prescient)
    (straight-use-package 'ivy-prescient)
    (straight-use-package 'company-prescient)
    (straight-use-package 'selectrum-prescient)

However, you may install using any other package manager if you
prefer.

## Usage

* To cause [Ivy] to use `prescient.el` sorting and filtering, enable
  `ivy-prescient-mode`.
* To cause [Company] to use `prescient.el` sorting, enable
  `company-prescient-mode`.
* To cause [Selectrum] to use `prescient.el` sorting and filtering,
  enable `selectrum-prescient-mode`.
* To cause your usage statistics to be saved between Emacs sessions,
  enable `prescient-persist-mode`.

Please note that **you must load Counsel before `ivy-prescient.el`**.
This is because loading Counsel results in a number of changes being
made to the user options of Ivy, which `ivy-prescient.el` must then
undo.

## Algorithm

`prescient.el` takes as input a list of candidates, and a query that
you type. The query is first split on spaces into subqueries (two
consecutive spaces match a literal space). Each subquery filters the
candidates because it must match as either a substring of the
candidate, a regexp, or an initialism (e.g. `ffap` matches
`find-file-at-point`, and so does `fa`). The last few candidates you
selected are displayed first, followed by the most frequently selected
ones, and then the remaining candidates are sorted by length. If you
don't like the algorithm used for filtering, you can choose a
different one by customizing `prescient-filter-method`.

## Configuration

* `prescient-history-length`: The number of recently selected
  candidates that are remembered and displayed at the top of the list.

* `prescient-frequency-decay`: `prescient.el` keeps a "frequency" for
  each selected candidate, which is incremented by one each time you
  select the candidate. To keep things tidy, frequencies are
  multiplied by this variable's value each time you select a new
  candidate, so they decrease over time.

* `prescient-frequency-threshold`: Once the frequency for an
  infrequently used command falls below the value of this variable,
  `prescient.el` forgets about it.

* `prescient-save-file`: Where to save statistics that are persisted
  between Emacs sessions when `prescient-persist-mode` is active. The
  default value follows the conventions of
  [`no-littering`][no-littering].

* `prescient-filter-method`: A list of algorithms to use for filtering
  candidates. The default is `literal`, `regexp`, and `initialism` as
  described above, but you can also use substring matching, initialism
  matching, regexp matching, fuzzy matching, prefix matching, or any
  combination of those. See the docstring for full details.

### Ivy-Specific
The following user options are specific to using Prescient with Ivy:

* `ivy-prescient-sort-commands`: By default, all commands have their
  candidates sorted. You can override this behavior by customizing
  `ivy-prescient-sort-commands`. See the docstring.

* `ivy-prescient-retain-classic-highlighting`: By default, the
  highlighting behavior of `ivy-prescient.el` is slightly different
  from Ivy's highlighting for `ivy--regex-ignore-order`. You can
  recover the original behavior by customizing this user option; see
  the docstring for more details.

* `ivy-prescient-enable-filtering`: If set to nil, then
  `ivy-prescient.el` does not apply `prescient.el` filtering to Ivy.
  See the Ivy documentation for information on how Ivy filters by
  default, and how to customize it manually.

* `ivy-prescient-enable-sorting`: If set to nil, then
  `ivy-prescient.el` does not apply `prescient.el` sorting to Ivy. See
  the Ivy documentation for information on how Ivy sorts by default,
  and how to customize it manually.

### Selectrum-Specific
In Selectrum, commands for toggling the filter methods used are bound
to a `M-s` prefix. This is similar to toggling commands used with
Isearch, such as `isearch-toggle-char-fold`, which is bound to `M-s '`
in `isearch-mode-map`. These Selectrum commands are bound in
`selectrum-prescient-filter-toggle-map`, and are:

| Key   | Command                               |
|-------|---------------------------------------|
| M-s f | selectrum-prescient-toggle-fuzzy      |
| M-s i | selectrum-prescient-toggle-initialism |
| M-s l | selectrum-prescient-toggle-literal    |
| M-s p | selectrum-prescient-toggle-prefix     |
| M-s r | selectrum-prescient-toggle-regexp     |

For example, if you wish to disable regexp filtering while inputting
a complex string (maybe to search for it literally), you can press
`M-s r` to toggle whether Selectrum uses Prescient's regexp
filtering. If you wish to use *only* regexp filtering, you can use
`C-u M-s r` to temporarily disable all other filter methods.

This toggling only applies to the current Selectrum buffer, and
doesn't affect the default filter settings (determined by
`prescient-filter-method`).

## Contributor guide

Please see [the contributor guide for my
projects](https://github.com/raxod502/contributor-guide).

[company]: https://github.com/company-mode/company-mode
[company-statistics]: https://github.com/company-mode/company-statistics
[counsel]: https://github.com/abo-abo/swiper#counsel
[flx]: https://github.com/lewang/flx
[helm]: https://github.com/emacs-helm/helm
[historian]: https://github.com/PythonNut/historian.el
[ido]: https://www.gnu.org/software/emacs/manual/ido.html
[ivy]: https://github.com/abo-abo/swiper#ivy
[ivy-release]: https://github.com/abo-abo/swiper/issues/1664
[no-littering]: https://github.com/emacscollective/no-littering
[selectrum]: https://github.com/raxod502/selectrum
[smex]: https://github.com/nonsequitur/smex
[straight.el]: https://github.com/raxod502/straight.el
