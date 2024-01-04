;; This file contains stub definitions from compat.el which allows
;; files to be byte-compiled in the absence of compat.el.

(unless (fboundp 'file-name-split)
  (defun file-name-split (_FILENAME)))

(provide 'compat)
