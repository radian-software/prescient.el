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

`prescient.el` is available on MELPA as three separate packages:

* [`prescient`](https://melpa.org/#/prescient)
* [`ivy-prescient`](https://melpa.org/#/ivy-prescient)
* [`company-prescient`](https://melpa.org/#/company-prescient)

The easiest way to install these packages is using
[`straight.el`][straight.el]:

    (straight-use-package 'prescient)
    (straight-use-package 'ivy-prescient)
    (straight-use-package 'company-prescient)

However, you may install using any other package manager if you
prefer.

## Usage

To cause [Ivy] to use `prescient.el` sorting and filtering, enable
`ivy-prescient-mode`. To cause [Company] to use `prescient.el`
sorting, enable `company-prescient-mode`. To cause your usage
statistics to be saved between Emacs sessions, enable
`prescient-persist-mode`.

## Algorithm

`prescient.el` takes as input a list of candidates, and a query that
you type. The query is first split on spaces into subqueries (two
consecutive spaces match a literal space). Each subquery filters the
candidates because it must match as either a substring of the
candidate or as an initialism (e.g. `ffap` matches
`find-file-at-point`, and so does `fa`). The last few candidates you
selected are displayed first, followed by the most frequently selected
ones, and then the remaining candidates are sorted by length.

If you don't like the algorithm used for filtering, you can choose a
different one by customizing `prescient-filter-method`. For Ivy, you
can set up keybindings for changing this value on the fly by
customizing `ivy-prescient-filter-method-keys`; by default, you can
toggle between substring/initialism matching and regexp matching by
pressing `C-c C-r`.

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

* `prescient-filter-method`: Which algorithm to use for filtering
  candidates. The default is `literal+initialism` as described above,
  but you can also use substring matching, initialism matching, regexp
  matching, or fuzzy matching. See the docstring for full details.

* `ivy-prescient-excluded-commands`: Some commands, like `swiper`,
  don't benefit from `prescient.el` sorting, so their usage statistics
  just pollute the save file. You can tell `prescient.el` about them
  here.

* `ivy-prescient-sort-commands`: Some [Counsel] commands, like
  `counsel-find-library`, intentionally disable sorting for their
  candidates. You can override this preference and re-enable sorting
  by adding such commands here. (To check if a command disables
  sorting, inspect its source code and see if it calls `ivy-read` with
  a nil value for the `:sort` keyword argument.)

* `ivy-prescient-filter-method-keys`: This is an alist which
  `ivy-prescient.el` uses to establish additional temporary bindings
  in `ivy-minibuffer-map`. These bindings allow you to quickly change
  the value of `prescient-filter-method` while inside Ivy. See the
  docstring for information about the format of this alist.

* `ivy-prescient-persist-filter-method`: If non-nil, then changes made
  to `prescient-filter-method` while Ivy is active persist until the
  next time you use Ivy, instead of being reset once you leave.

* `ivy-prescient-retain-classic-highlighting`: By default, the
  highlighting behavior of `ivy-prescient.el` is slightly different
  from Ivy's highlighting for `ivy--regex-ignore-order`. You can
  recover the original behavior by customizing this user option; see
  the docstring for more details.

* `ivy-prescient-enable-filtering`: If non-nil, filter Ivy collections
  with `prescient-filter-method`. You can set this to nil to retain
  default filter functions set by ivy.

* `ivy-prescient-enable-sorting`: If non-nil, use adapative sorting
  for ivy collections. You can set this to nil to retain default ivy
  sorting.

## Known bugs

If you are using release 0.10.0 of Ivy, you will notice a number of
bugs and significantly degraded functionality. To fix these, please
upgrade to release 0.11.0.

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
[smex]: https://github.com/nonsequitur/smex
[straight.el]: https://github.com/raxod502/straight.el
