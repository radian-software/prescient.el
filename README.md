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

Unfortunately I do not like any of these, for a variety of reasons.
`prescient.el` attempts to find the best of all worlds. It uses the
high-quality and predictable sorting algorithm of Smex, but
generalizes it to work on all commands rather than just `M-x`.
Furthermore, it uses the more sophisticated interface of Ivy rather
than IDO. Fuzzy-matching is supported, but unlike Flx only initialisms
are matched; this eliminates the unpredictability of Flx. The idea is
to get something that, like Helm, works nicely out of the box in all
contexts, but that, unlike Helm, is simple and lightweight. By
reimplementing frequency and usage tracking, the need for dependencies
on Historian and Flx are eliminated, leading to tighter code.
`prescient.el` also supports Company sorting and filtering, thus
eliminating the need for a separate Company-Statistics package.

## How it works

* `prescient.el` uses a very simple algorithm. The first few choices
  are the last selections you made. After that, candidates are sorted
  first by frequency of usage and then by length.

* Filtering is done by substring and initialism matching. This means
  that your query must match an exact substring of a candidate, or an
  exact substring of the initials of a candidate, or that candidate is
  discarded.

* To filter candidates further, simply enter another query, separated
  from the first by a space. The queries can match in any order. You
  can enter a literal space using two consecutive spaces.

## Usage

Cause Ivy to use `prescient.el` sorting and filtering unless otherwise
specified in `ivy-re-builders-alist` and `ivy-sort-functions-alist`:

    (ivy-prescient-mode +1)

Cause statistics to be saved between Emacs sessions:

    (prescient-persist-mode +1)

[company-statistics]: https://github.com/company-mode/company-statistics
[flx]: https://github.com/lewang/flx
[helm]: https://github.com/emacs-helm/helm
[historian]: https://github.com/PythonNut/historian.el
[ido]: https://www.gnu.org/software/emacs/manual/ido.html
[ivy]: https://github.com/abo-abo/swiper#ivy
[smex]: https://github.com/nonsequitur/smex
[straight.el]: https://github.com/raxod502/straight.el
