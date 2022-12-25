;;; corfu-prescient.el --- prescient.el + Corfu -*- lexical-binding: t -*-

;; Copyright (C) 2022 Radian LLC and contributors

;; Author: Radian LLC <contact+prescient@radian.codes>
;; Homepage: https://github.com/radian-software/prescient.el
;; Keywords: extensions
;; Created: 23 Sep 2022
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; corfu-prescient.el provides an interface for using prescient.el
;; to sort and filter candidates in Corfu menus. To enable its
;; functionality, turn on `corfu-prescient-mode' in your init-file
;; or interactively.

;; For more information, see https://github.com/radian-software/prescient.el.

;;; Code:

;;;; Libraries and Declarations

(require 'cl-lib)
(require 'corfu nil t)
(require 'prescient)
(require 'subr-x)

(defvar corfu--candidates)
(defvar corfu--index)
(defvar corfu--input)
(defvar corfu--state-vars)
(defvar corfu-map)
(defvar corfu-mode)
(defvar corfu-sort-function)
(defvar corfu-sort-override-function)
(declare-function corfu--update "ext:corfu" (&optional interruptible))

;;;; Customization

(defgroup corfu-prescient nil
  "Prescient adapter for Corfu."
  :group 'convenience
  :prefix "corfu-prescient"
  :link '(url-link "https://github.com/radian-software/prescient.el"))

(defcustom corfu-prescient-enable-filtering t
  "Whether the `prescient' completion style is used in Corfu."
  :type 'boolean
  :group 'corfu-prescient)

(defcustom corfu-prescient-enable-sorting t
  "Whether prescient.el sorting is used in Corfu."
  :type 'boolean
  :group 'corfu-prescient)

(defcustom corfu-prescient-override-sorting nil
  "Whether to force sorting by `corfu-prescient'.

If non-nil, then `corfu-prescient-mode' sets
`corfu-sort-override-function' to the function
`prescient-completion-sort'.

Changing this variable will not take effect until
`corfu-prescient-mode' has been reloaded."
  :group 'corfu-prescient
  :type 'boolean)

(defcustom corfu-prescient-completion-styles
  prescient--completion-recommended-styles
  "The completion styles used by `corfu-prescient-mode'."
  :group 'corfu-prescient
  :type '(repeat symbol))

(defcustom corfu-prescient-completion-category-overrides
  prescient--completion-recommended-overrides
  "The completion-category overrides used by `corfu-prescient-mode'."
  :group 'corfu-prescient
  :type '(repeat (cons symbol (repeat (cons symbol (repeat symbol))))))

;;;; Toggling Commmands
(defun corfu-prescient--toggle-refresh ()
  "Refresh the Corfu UI.
This function is added to `prescient--toggle-refresh-functions'
by `corfu-prescient-mode'."
  (when corfu--input
    (setq corfu--input nil)
    (corfu--update)))

;;;; Minor mode

(defvar corfu-prescient--old-toggle-binding nil
  "Previous binding of `M-s' in `corfu-map'.")

(defun corfu-prescient--remember (&rest _)
  "Advice for remembering candidates in Corfu."
  (when (>= corfu--index 0)
    (prescient-remember
     (substring-no-properties
      (nth corfu--index corfu--candidates)))))

(defvar-local corfu-prescient--local-settings nil
  "Whether this buffer has local settings due to `corfu-prescient-mode'.")

(defun corfu-prescient--apply-completion-settings ()
  "Apply the local completion settings."
  (prescient--completion-make-vars-local)
  (prescient--completion-save-completion-settings)
  (prescient--completion-apply-completion-settings
   :styles corfu-prescient-completion-styles
   :overrides corfu-prescient-completion-category-overrides)
  (setq corfu-prescient--local-settings t))

(defun corfu-prescient--undo-completion-settings ()
  "Undo the local completion settings."
  (prescient--completion-restore-completion-settings
   :styles corfu-prescient-completion-styles
   :overrides corfu-prescient-completion-category-overrides)
  (prescient--completion-kill-local-vars)
  (setq corfu-prescient--local-settings nil))

(defun corfu-prescient--change-completion-settings (&rest _)
  "Apply or undo the local completion settings in `corfu-mode-hook'."
  (if corfu-mode
      (unless corfu-prescient--local-settings
        (corfu-prescient--apply-completion-settings))
    ;; We need this here in case `corfu-mode' itself is disabled. We
    ;; also need to undo things when `corfu-prescient-mode' is
    ;; disabled, which happens in the mode's definition.
    (when corfu-prescient--local-settings
      (corfu-prescient--undo-completion-settings))))

;;;###autoload
(define-minor-mode corfu-prescient-mode
  "Minor mode to use prescient.el in Corfu menus.

This mode will:

- if `corfu-prescient-override-sorting' is non-nil, override the
  function stored in `corfu-sort-override-function' via advice
  and set `corfu-prescient-enable-filtering' to t

- if `corfu-prescient-enable-filtering' is non-nil, override the
  function stored in `corfu-sort-function' via advice

- if `corfu-prescient-enable-filtering' is non-nil:
  - bind `prescient-toggle-map' to `M-s' in `corfu-map'
  - change `completion-stlyes' to `corfu-prescient-completion-styles'
  - apply `corfu-prescient-completion-category-overrides'
    to `completion-category-overrides'
  - set `completion-category-defaults' to nil

- advise `corfu-insert' to remember candidates"
  :global t
  :group 'prescient
  (if (not (featurep 'corfu))
      (progn
        (setq corfu-prescient-mode nil)
        (user-error "`corfu-prescient-mode': Corfu not found"))

    ;; Prevent messing up variables if we explicitly enable the
    ;; mode when it's already on.
    ;;
    ;; Undo sorting settings.
    (remove-function corfu-sort-function #'prescient-completion-sort)
    (remove-function corfu-sort-override-function #'prescient-completion-sort)

    ;; Unbind toggling commands and unhook refresh function.
    (when (equal (lookup-key corfu-map (kbd "M-s"))
                 prescient-toggle-map)
      (define-key corfu-map (kbd "M-s")
                  corfu-prescient--old-toggle-binding))
    (remove-hook 'prescient--toggle-refresh-functions
                 #'corfu-prescient--toggle-refresh)
    (cl-callf cl-set-difference corfu--state-vars
      prescient--toggle-vars
      :test #'eq)

    ;; Undo filtering settings.
    (remove-hook 'corfu-mode-hook
                 #'corfu-prescient--change-completion-settings)
    (dolist (b (buffer-list))
      (when (buffer-local-value 'corfu-prescient--local-settings b)
        (with-current-buffer b
          (corfu-prescient--undo-completion-settings))))

    ;; Undo remembrance settings.
    (advice-remove 'corfu-insert #'corfu-prescient--remember)

    ;; Once cleaned up, if enabling, add things back in.
    (when corfu-prescient-mode
      (when corfu-prescient-override-sorting
        (setq corfu-prescient-enable-sorting t)
        (add-function :override corfu-sort-override-function
                      #'prescient-completion-sort))

      (when corfu-prescient-enable-sorting
        (add-function :override corfu-sort-function
                      #'prescient-completion-sort))

      (when corfu-prescient-enable-filtering
        ;; Configure changing settings in the hook.
        (add-hook 'corfu-mode-hook
                  #'corfu-prescient--change-completion-settings)

        ;; Immediately apply the settings in buffers where
        ;; `corfu-mode' is already on.
        (dolist (b (buffer-list))
          (when (buffer-local-value corfu-mode b)
            (with-current-buffer b
              (corfu-prescient--apply-completion-settings))))

        ;; Bind toggling commands.
        (setq corfu-prescient--old-toggle-binding
              (lookup-key corfu-map (kbd "M-s")))
        (define-key corfu-map (kbd "M-s") prescient-toggle-map)

        ;; Make sure Corfu refreshes immediately.
        (add-hook 'prescient--toggle-refresh-functions
                  #'corfu-prescient--toggle-refresh)

        ;; Clean up the local versions of the toggling variables
        ;; after the Corfu pop-up closes. For the toggling vars, it
        ;; is the commands themselves that make the variables buffer
        ;; local.
        (cl-callf cl-union corfu--state-vars prescient--toggle-vars
                  :test #'eq))

      ;; While sorting might not be enabled in Corfu, it might
      ;; still be enabled in another UI, such as Selectrum or Vertico.
      ;; Therefore, we still want to remember candidates.
      (advice-add 'corfu--insert :before #'corfu-prescient--remember))))

(provide 'corfu-prescient)
;;; corfu-prescient.el ends here
