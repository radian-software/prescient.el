;;; company-prescient.el --- Prescient sorting for Company. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/prescient.el
;; Keywords: extensions
;; Created: 7 Aug 2017
;; Package-Requires: ((emacs "25.1") (prescient "1.0") (company "0.9.6"))
;; Version: 1.0

;;; Commentary:

;; This package is under construction.

;;; Code:

;;;; Libraries

(require 'company)
(require 'prescient)

(defalias 'company-prescient-transformer #'prescient-sort
  "Candidate transformer function that uses prescient.el to sort candidates.
This is for use in `company-transformers'.")

(defalias 'company-prescient-completion-finished #'prescient-remember
  "Hook function to remember selected Company candidate.
This is for use on `company-completion-finished-hook'.")

;;;###autoload
(define-minor-mode company-prescient-mode
  "Minor mode to use prescient.el in Company completions."
  :global t
  (if company-prescient-mode
      (progn
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
;; outline-regexp: ";;;;* "
;; End:
