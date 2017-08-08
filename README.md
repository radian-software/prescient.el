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

`prescient.el` uses a very simple algorithm. If you don't enter any
query, then the first few choices are the last selections you made.
After that, candidates are sorted first by frequency of usage and then
by length.

Once you enter a query, the same base sorting still applies. However,
if your query matches an exact substring of a candidate, then that
candidate is ranked higher. Alternatively (and exclusively), if your
query matches an exact substring of the initials of a candidate, then
that candidate is also ranked higher.

You can narrow further by providing multiple queries, separated by
spaces. Substring and initial matching will apply separately for each
query, allowing you to quickly select the candidate you want. Of
course, once you've done so once, that candidate will be right at the
top of the list for next time.

[company-statistics]: https://github.com/company-mode/company-statistics
[flx]: https://github.com/lewang/flx
[helm]: https://github.com/emacs-helm/helm
[historian]: https://github.com/PythonNut/historian.el
[ido]: https://www.gnu.org/software/emacs/manual/ido.html
[ivy]: https://github.com/abo-abo/swiper#ivy
[smex]: https://github.com/nonsequitur/smex
