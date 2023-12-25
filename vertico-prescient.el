;;; vertico-prescient.el --- prescient.el + Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2022 Radian LLC and contributors

;; Author: Radian LLC <contact+prescient@radian.codes>
;; Homepage: https://github.com/radian-software/prescient.el
;; Keywords: extensions
;; Created: 23 Sep 2022
;; Package-Requires: ((emacs "27.1") (prescient "6.1.0") (vertico "0.28") (compat "29.1"))
;; SPDX-License-Identifier: MIT
;; Version: 6.2.0

;;; Commentary:

;; vertico-prescient.el provides an interface for using prescient.el
;; to sort and filter candidates in Vertico menus. To enable its
;; functionality, turn on `vertico-prescient-mode' in your init-file
;; or interactively.

;; For more information, see https://github.com/radian-software/prescient.el.

;;; Code:

;;;; Libraries and Declarations

(eval-when-compile (require 'cl-lib))
(require 'compat)
(require 'prescient)
(require 'subr-x)
(require 'vertico)

(defvar vertico-mode)

;;;; Customization

(defgroup vertico-prescient nil
  "Prescient adapter for Vertico."
  :group 'convenience
  :prefix "vertico-prescient"
  :link '(url-link "https://github.com/radian-software/prescient.el"))

(defcustom vertico-prescient-enable-filtering t
  "Whether the `prescient' completion style is used in Vertico."
  :type 'boolean
  :group 'vertico-prescient)

(defcustom vertico-prescient-enable-sorting t
  "Whether the `prescient' completion style is used in Vertico."
  :type 'boolean
  :group 'vertico-prescient)

(defcustom vertico-prescient-override-sorting nil
  "Whether to force sorting by `vertico-prescient'.

If non-nil, then `vertico-prescient-mode' sets
`vertico-sort-override-function' to the function
`prescient-completion-sort'.

Changing this variable will not take effect until
`vertico-prescient-mode' has been reloaded."
  :group 'vertico-prescient
  :type 'boolean)

(defcustom vertico-prescient-completion-styles
  prescient--completion-recommended-styles
  "The completion styles used by `vertico-prescient-mode'."
  :group 'vertico-prescient
  :type '(repeat symbol))

(defcustom vertico-prescient-completion-category-overrides
  prescient--completion-recommended-overrides
  "The completion-category overrides used by `vertico-prescient-mode'."
  :group 'vertico-prescient
  :type '(repeat (cons symbol (repeat (cons symbol (repeat symbol))))))

;;;; Toggling commands
(defun vertico-prescient--toggle-refresh ()
  "Refresh to Vertico UI.
This function is added to `prescient--toggle-refresh-functions'
by `vertico-prescient-mode'."
  (when vertico--input
    (setq vertico--input t
          vertico--history-hash nil
          vertico--lock-candidate nil)
    (vertico--exhibit)))

;;;; Minor mode

(defvar vertico-prescient--old-sort-function nil
  "Previous value of `vertico-sort-function'.")

(defvar vertico-prescient--old-sort-override-function nil
  "Previous value of `vertico-sort-override-function'.")

(defvar vertico-prescient--old-toggle-binding nil
  "Previous binding of `M-s' in `vertico-map'.")

(defvar-local vertico-prescient--local-settings nil
  "Whether this buffer has local settings due to `vertico-prescient-mode'.")

(defun vertico-prescient--apply-completion-settings ()
  "Apply the local completion settings after `vertico--setup'."
  (unless vertico-prescient--local-settings
    (prescient--completion-make-vars-local)
    (prescient--completion-save-completion-settings)
    (prescient--completion-apply-completion-settings
     :styles vertico-prescient-completion-styles
     :overrides vertico-prescient-completion-category-overrides)
    (setq vertico-prescient--local-settings t)))

(defun vertico-prescient--undo-completion-settings ()
  "Undo the local completion settings."
  (prescient--completion-restore-completion-settings
   :styles vertico-prescient-completion-styles
   :overrides vertico-prescient-completion-category-overrides)
  (prescient--completion-kill-local-vars)
  (setq vertico-prescient--local-settings nil))

(defun vertico-prescient--remember-minibuffer-contents ()
  "Remember the minibuffer contents as a candidate.

If we are not completing a file name (according to
`minibuffer-completing-file-name'), then we remember the
minibuffer contents. When completing file names, we remember the
last component of the completed file name path, including a
trailing directory separator as needed."
  (let ((txt (minibuffer-contents-no-properties)))
    (unless (string-empty-p txt)
      (prescient-remember (if minibuffer-completing-file-name
                              (if (directory-name-p txt)
                                  (thread-first txt
                                                file-name-split
                                                (last 2)
                                                car
                                                file-name-as-directory)
                                (thread-first txt
                                              file-name-split
                                              last
                                              car))
                            txt)))))

(defvar vertico-prescient--insertion-commands
  '(vertico-insert
    vertico-quick-insert
    vertico-directory-enter)
  "Commands that trigger `prescient-remember' after running.")

(defun vertico-prescient--remember-inserted-candidate ()
  "Remember the minibuffer contents as a candidate after insertion commands.

Such commands are listed in `vertico-prescient--insertion-commands'."
  (when (and (memq this-command vertico-prescient--insertion-commands)
             ;; Pro-actively try to avoid running the remembrance
             ;; function twice for commands that can insert and exit.
             ;; This might not be needed?
             (let ((buf (current-buffer)))
               ;; In Emacs 28+, we would use the new second argument
               ;; to `minibufferp' check if the buffer is the active
               ;; minibuffer, but we want to support Emacs 27.
               (and (> 0 (minibuffer-depth))
                    (minibufferp buf)
                    (eq (active-minibuffer-window)
                        (get-buffer-window buf)))))
    (vertico-prescient--remember-minibuffer-contents)))

(defvar vertico-prescient--exit-commands
  '(vertico-exit
    vertico-quick-exit
    exit-minibuffer
    vertico-directory-enter)
  "Commands that trigger `prescient-remember' when exiting the minibuffer.")

(defun vertico-prescient--remember-exited-candidate ()
  "Remember the minibuffer contents as a candidate after exiting commands.

Such commands are listed in `vertico-prescient--exit-commands'."
  (when (memq this-command vertico-prescient--exit-commands)
    (vertico-prescient--remember-minibuffer-contents)))

(defun vertico-prescient--setup-remembrance ()
  "Set up the remembering functions in the relevant hooks."
  (when vertico-mode
    (add-hook 'post-command-hook
              #'vertico-prescient--remember-inserted-candidate
              nil t)
    (add-hook 'minibuffer-exit-hook
              #'vertico-prescient--remember-exited-candidate
              nil t)))

;;;###autoload
(define-minor-mode vertico-prescient-mode
  "Minor mode to use prescient.el in Vertico menus.

This mode will:
- if `vertico-prescient-override-sorting' is non-nil,
  configure `vertico-sort-override-function' and set
 `vertico-prescient-enable-filtering' to t

- if `vertico-prescient-enable-filtering' is non-nil,
  configure `vertico-sort-function'

- if `vertico-prescient-enable-filtering' is non-nil:
  - bind `prescient-toggle-map' to `M-s' in `vertico-map'
  - change `completion-styles' to `vertico-prescient-completion-styles'
  - apply `vertico-prescient-completion-category-overrides'
    to `completion-category-overrides'
  - set `completion-category-defaults' to nil

- insert code for remembering candidates into `minibuffer-exit-hook'
  and `post-command-hook'."
  :global t
  :group 'prescient
  (if vertico-prescient-mode
      ;; Turn on the mode.
      (progn
        ;; Prevent messing up variables if we explicitly enable the
        ;; mode when it's already on.
        (vertico-prescient-mode -1)
        (setq vertico-prescient-mode t)

        (when vertico-prescient-override-sorting
          (setq vertico-prescient-enable-sorting t)
          (cl-shiftf vertico-prescient--old-sort-override-function
                     vertico-sort-override-function
                     #'prescient-completion-sort))

        (when vertico-prescient-enable-sorting
          (cl-shiftf vertico-prescient--old-sort-function
                     vertico-sort-function
                     #'prescient-completion-sort))

        (when vertico-prescient-enable-filtering
          ;; Configure completion settings.
          (advice-add 'vertico--setup
                      :after #'vertico-prescient--apply-completion-settings)

          ;; Bind toggling commands.
          (setq vertico-prescient--old-toggle-binding
                (lookup-key vertico-map (kbd "M-s")))
          (define-key vertico-map (kbd "M-s") prescient-toggle-map)

          ;; Make sure Vertico refreshes immediately.
          (add-hook 'prescient--toggle-refresh-functions
                    #'vertico-prescient--toggle-refresh))

        ;; While sorting might not be enabled in Vertico, it might
        ;; still be enabled in another UI, such as Company or Corfu.
        ;; Therefore, we still want to remember candidates.
        (add-hook 'minibuffer-setup-hook
                  #'vertico-prescient--setup-remembrance))

    ;; Turn off mode.

    ;; Undo sorting settings.
    (when (eq vertico-sort-function #'prescient-completion-sort)
      (setq vertico-sort-function vertico-prescient--old-sort-function))
    (when (eq vertico-sort-override-function #'prescient-completion-sort)
      (setq vertico-sort-override-function
            vertico-prescient--old-sort-override-function))

    ;; Unbind toggling commands and unhook refresh function.
    (when (equal (lookup-key vertico-map (kbd "M-s"))
                 prescient-toggle-map)
      (define-key vertico-map (kbd "M-s")
                  vertico-prescient--old-toggle-binding))
    (remove-hook 'prescient--toggle-refresh-functions
                 #'vertico-prescient--toggle-refresh)

    ;; Undo filtering settings.
    (advice-remove 'vertico--setup
                   #'vertico-prescient--apply-completion-settings)
    (dolist (b (buffer-list))
      (when (buffer-local-value 'vertico-prescient--local-settings b)
        (with-current-buffer b
          (vertico-prescient--undo-completion-settings))))

    ;; Undo remembrance settings.
    (remove-hook 'minibuffer-setup-hook
                 #'vertico-prescient--setup-remembrance)))

(provide 'vertico-prescient)
;;; vertico-prescient.el ends here

;; LocalWords:  Vertico's
