;;; prescient.el --- Smart/stupid candidate sorting. -*- lexical-binding: t -*-

;; Copyright (C) 2017 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 7 Aug 2017

;;; Commentary:

;; This package is under construction.

;;; Code:

;;;; Customizable variables

(defvar prescient-history-length 10
  "Number of recently chosen candidates that will be remembered.
This is a count per completion context, not a global limit.")

(defvar prescient-frequency-count 20
  "Number of frequently chosen candidates that will be remembered.
This is a count per completion context, not a global limit.")

(defvar prescient-history-bonus 100
  "Bonus for recently chosen candidates.
The oldest candidate in the recent history will be given this
bonus; the second oldest will be given twice this bonus, and so
on. Thus, the maximum bonus depends on the setting of
`prescient-history-length'.")

(defvar prescient-frequency-bonus 10
  "Bonus for frequently chosen candidates.
The least frequent candidate in the frequency tracker will be
given this bonus; the second least frequent will be given twice
this bonus, and so on. Thus, the maximum bonus depends on the
setting of `prescient-frequency-count'.")

(defvar prescient-prefix-bonus 100
  "Bonus if query matches at beginning of candidate.")

(defvar prescient-basename-bonus 100
  "Bonus if query matches basename of path rather than a stem component.")

(defvar prescient-in-order-bonus 100
  "Bonus if queries match candidate in order rather than out of order.")

;;;; Closing remarks

(provide 'prescient)

;;; prescient.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
