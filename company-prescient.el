;;; company-prescient.el --- prescient.el + Company -*- lexical-binding: t -*-

;; Copyright (C) 2018 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 7 May 2018
;; Package-Requires: ((emacs "25.1") (prescient "5.0") (company "0.9.6"))
;; SPDX-License-Identifier: MIT
;; Version: 5.0

;;; Commentary:

;; company-prescient.el provides an interface for using prescient.el
;; to sort Company completions. To enable its functionality, turn on
;; `company-prescient-mode' in your init-file or interactively.

;; Note that company-prescient.el does not change the filtering
;; behavior of Company. This is because that can't be done without
;; updating each Company backend individually.

;; For more information, see https://github.com/raxod502/prescient.el.

;;; Code:

;;;; Libraries

(require 'company)
(require 'prescient)

;;;; User options

(defcustom company-prescient-sort-length-enable :default
  "Whether to sort candidates by length in Company.
The value of `prescient-sort-length-enable' is bound to the value
of this variable when sorting Company candidates. If the value of
this variable is `:default', then this binding is skipped."
  :group 'prescient
  :type 'boolean)

;;;; Minor mode

(defun company-prescient-transformer (candidates)
  "Candidate transformer function that uses prescient.el to sort CANDIDATES.
This is for use in `company-transformers'."
  (let ((prescient-sort-length-enable
         (if (eq company-prescient-sort-length-enable :default)
             prescient-sort-length-enable
           company-prescient-sort-length-enable)))
    (prescient-sort candidates)))

(defalias 'company-prescient-completion-finished #'prescient-remember
  "Hook function to remember selected Company candidate.
This is for use on `company-completion-finished-hook'.")

;;;###autoload
(define-minor-mode company-prescient-mode
  "Minor mode to use prescient.el in Company completions."
  :global t
  :group 'prescient
  (if company-prescient-mode
      (progn
        (company-prescient-mode -1)
        (setq company-prescient-mode t)
        (add-to-list 'company-transformers #'company-prescient-transformer)
        (add-hook 'company-completion-finished-hook
                  #'company-prescient-completion-finished))
    (setq company-transformers
          (delq #'company-prescient-transformer company-transformers))
    (remove-hook 'company-completion-finished-hook
                 #'company-prescient-completion-finished)))

;;;; Closing remarks

(provide 'company-prescient)

;;; company-prescient.el ends here

;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; outline-regexp: ";;;;* "
;; End:
