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

## Configuration and other features

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
  matching, regexp matching, fuzzy matching, prefix matching, anchored
  matching, literal-prefix matching, or any combination of those. See
  the docstring for full details.

* `prescient-filter-alist`: An alist of symbol-function pairs that
  associate a symbol in `prescient-filter-method` with a function that
  creates a regexp for matching a candidate. You can add to this alist
  to define your own custom filter methods, and use them by adding the
  appropriate symbol to `prescient-filter-method`.

* `prescient-sort-full-matches-first`: Whether `prescient.el` sorts
  candidates that are fully matched before candidates that are
  partially matched.

* `prescient-use-char-folding`: Whether the `literal` and
  `literal-prefix` filter methods use character folding.

### Company-specific

* `company-prescient-sort-length-enable`: By default, the standard
  `prescient.el` sorting algorithm is used for all Company completions
  when `company-prescient-mode` is enabled. However, this algorithm is
  inappropriate in some situations. In particular, some Company
  backends return fuzzy-matched candidates with an intelligent sorting
  pre-applied. In this case, the fallback sorting by length that
  `prescient.el` does will just make a giant mess of things. By
  customizing this user option to nil for such Company backends (see
  [Radian][radian-sort-length-enable] for an example), you can avoid
  the problem. Then `prescient.el` will helpfully move recently and
  frequently used candidates to the top of the completions list, but
  otherwise leave candidate ordering alone.

### Ivy-specific
The following user options are specific to using `prescient.el` with
Ivy:

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

### Selectrum-specific
`selectrum-prescient.el` provides special commands (see the table
below) to adjust how `prescient.el` filters candidates in the current
Selectrum buffer.

For example, to toggle regexp filtering on or off (perhaps you're
searching for a long/complex candidate), you can press `M-s r`. If you
wish to use *only* regexp filtering, you can use `C-u M-s r` to
unconditionally turn on regexp filtering and turn off all other
methods. This toggling is a buffer-local effect, and does not change
the default filter behavior. For that, customize
`prescient-filter-method`.

These commands are similar in usage to Isearch's own toggling
commands, except that multiple filtering methods can be active at the
same time. While `selectrum-prescient-mode` is enabled, `M-s` is bound
to `selectrum-prescient-toggle-map` in the Selectrum buffer, and is
used as a prefix key to access the commands.

| Key     | Command                                     |
|---------|---------------------------------------------|
| `M-s a` | `selectrum-prescient-toggle-anchored`       |
| `M-s f` | `selectrum-prescient-toggle-fuzzy`          |
| `M-s i` | `selectrum-prescient-toggle-initialism`     |
| `M-s l` | `selectrum-prescient-toggle-literal`        |
| `M-s p` | `selectrum-prescient-toggle-prefix`         |
| `M-s P` | `selectrum-prescient-toggle-literal-prefix` |
| `M-s r` | `selectrum-prescient-toggle-regexp`         |
| `M-s '` | `selectrum-prescient-toggle-char-fold`      |

When defining custom filter methods, you can create new bindings using
`selectrum-prescient-create-and-bind-toggle-command`, which takes an
unquoted filter symbol and a string that can be used by `kbd`. For
example,

```emacs-lisp
(selectrum-prescient-create-and-bind-toggle-command my-foo "M-f")
```

will bind a command to toggle the `my-foo` filter to `M-s M-f`.

The part of each candidate that matches your input is highlighted with
the face `selectrum-prescient-primary-highlight`. There is also
`selectrum-prescient-secondary-highlight` for additional highlighting of
specific matched parts of the input.

The following example shows customizing these faces, I use the
[Zerodark](https://github.com/NicolasPetton/zerodark-theme) color
theme, which includes colors for Ivy, but not for Selectrum. I
inspected the theme source code to see what colors were being used for
Ivy, and copied them to be used for Selectrum as well:

```elisp
(require 'zerodark-theme)

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'zerodark
   `(selectrum-current-candidate
     ((,class (:background "#48384c"
                           :weight bold
                           :foreground "#c678dd"))))
   `(selectrum-prescient-primary-highlight
   ((,class (:foreground "#da8548"))))
   `(selectrum-prescient-secondary-highlight
   ((,class (:foreground "#98be65"))))))

(enable-theme 'zerodark)
```

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
[radian-sort-length-enable]: https://github.com/raxod502/radian/blob/06f2a30258deacb719a195221747ce1d528fc2fe/emacs/radian.el#L2513-L2532
[selectrum]: https://github.com/raxod502/selectrum
[smex]: https://github.com/nonsequitur/smex
[straight.el]: https://github.com/raxod502/straight.el
