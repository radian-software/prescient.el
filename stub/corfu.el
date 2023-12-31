;; This file contains stub definitions from corfu.el which allow
;; corfu-prescient.el to be byte-compiled in the absence of
;; corfu.el.

(defvar corfu--candidates nil)
(defvar corfu--index nil)
(defvar corfu--initial-state nil)
(defvar corfu--input nil)
;; Removed https://github.com/minad/corfu/commit/63d1de2696adcb09a4ea01ba668635364e37a9c2
(defvar corfu--state-vars nil)
(defvar corfu-map nil)
(defvar corfu-mode nil)
(defvar corfu-sort-function nil)
(defvar corfu-sort-override-function nil)
(defun corfu--update (&optional interruptible))

(provide 'corfu)
