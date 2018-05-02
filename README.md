**prescient.el:** because sometimes a dumb algorithm is the smartest
in the end.

## Motivation

There are many packages which provide some support for selecting
quickly from a list of candidates provided by some completion
framework. For example:

* [Company-Statistics]
* [Flx]
* [Helm]
* [Historian]
* [IDO]
* [Ivy]
* [Smex]

Unfortunately I do not like any of these. Each one is either too big
and does too much magic, or is too limited in scope to be useful by
itself. Note, this is only my personal taste in sorting algorithms; I
have the highest respect for all of the authors.

## How it works

`prescient.el` uses a very simple algorithm. The first few choices are
the last selections you made. After that, candidates are sorted first
by frequency of usage and then by length.

Filtering is done by substring and initialism matching. This means
that your query must match an exact substring of a candidate, or an
exact substring of the initials of a candidate, or that candidate is
discarded.

To filter candidates further, simply enter another query, separated
from the first by a space. The queries can match in any order. You can
enter a literal space using two consecutive spaces.

[company-statistics]: https://github.com/company-mode/company-statistics
[flx]: https://github.com/lewang/flx
[helm]: https://github.com/emacs-helm/helm
[historian]: https://github.com/PythonNut/historian.el
[ido]: https://www.gnu.org/software/emacs/manual/ido.html
[ivy]: https://github.com/abo-abo/swiper#ivy
[smex]: https://github.com/nonsequitur/smex
