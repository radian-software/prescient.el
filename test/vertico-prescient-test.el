;;; vertico-prescient-test.el --- ERT tests for vertico-prescient -*- lexical-binding: t -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for `vertico-prescient'.  Run with `make test'.  Vertico is
;; not available in the test environment, so the project's `stub/'
;; directory is added to the load path (matches `make compile').

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Forward declaration: in older Emacs releases `read-passwd-mode' is
;; not yet defined, but the password-prompt detection still needs to
;; treat the symbol as special so `bound-and-true-p' / `let' work.
(require 'auth-source)
(defvar read-passwd-mode nil)

(require 'vertico-prescient)

(ert-deftest vertico-prescient--password-prompt-p/read-passwd-mode ()
  "Detects an active `read-passwd-mode'."
  (with-temp-buffer
    (let ((read-passwd-mode t))
      (should (vertico-prescient--password-prompt-p)))))

(ert-deftest vertico-prescient--password-prompt-p/read-passwd-map ()
  "Detects `read-passwd-map' as the local keymap."
  (skip-unless (boundp 'read-passwd-map))
  (with-temp-buffer
    (use-local-map read-passwd-map)
    (should (vertico-prescient--password-prompt-p))))

(ert-deftest vertico-prescient--password-prompt-p/default-nil ()
  "Returns nil when no password indicators are present."
  (with-temp-buffer
    (let ((read-passwd-mode nil))
      (should-not (vertico-prescient--password-prompt-p)))))

(ert-deftest vertico-prescient--remember-minibuffer-contents/skips-passwords ()
  "Does not call `prescient-remember' when in a password prompt."
  (with-temp-buffer
    (insert "supersecret")
    (let ((read-passwd-mode t)
          (calls 0))
      (cl-letf (((symbol-function 'prescient-remember)
                 (lambda (&rest _) (cl-incf calls))))
        (vertico-prescient--remember-minibuffer-contents))
      (should (= calls 0)))))

(ert-deftest vertico-prescient--remember-minibuffer-contents/records-otherwise ()
  "Calls `prescient-remember' for non-password minibuffer contents."
  (with-temp-buffer
    (insert "ordinary-candidate")
    (let ((read-passwd-mode nil)
          (minibuffer-completing-file-name nil)
          (recorded nil))
      (cl-letf (((symbol-function 'prescient-remember)
                 (lambda (cand &rest _) (setq recorded cand))))
        (vertico-prescient--remember-minibuffer-contents))
      (should (equal recorded "ordinary-candidate")))))

(provide 'vertico-prescient-test)
;;; vertico-prescient-test.el ends here
