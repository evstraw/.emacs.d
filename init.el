;; -*- lexical-binding: t; -*-

;; --------------
;; Disable all mouse-interactive interfaces early.
;; --------------
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; ----------------
;; Temporarily raise GC threshold while loading
;; ----------------
(setq gc-cons-threshold most-positive-fixnum)

;; ----------------
;; Configure package sources
;; ----------------
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives
	       (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives
  ;;              (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize t)
;;(package-refresh-contents)
(eval-when-compile
  (require 'use-package))

;; ---------------
;; Keep generated settings in a separate file.
;; ---------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defvar modules-dir
      (expand-file-name "modules" user-emacs-directory))
(defvar thirdparty-dir
      (expand-file-name "thirdparty" user-emacs-directory))

(add-to-list 'load-path modules-dir)
(add-to-list 'load-path thirdparty-dir)

(defun open-init-file ()
  "Opens the init.el file quickly."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key "\C-cd" 'open-dotfile)

(let ((machine-select-file (expand-file-name "machine-select.el" modules-dir))
      (machine-select-contents
       ";;; machine-select -- Allows user to specify machine for config
;;; Commentary:
;;; This package defines one constant, MACHINE-SELECT-MACHINE, that allows the user to specify
;;; which machine Emacs is currently running on, and allows the configuration to make different
;;; choices based on the current machine.  For example, my desktop has a different screen size
;;; than my laptop, so I would like a different font size on each machine.
;;; Note that this file SHOULD NOT be backed up to version control or synchronized between
;;; machines, unless you want them to have the same behavior; it will be created automatically
;;; by the configuration if it does not exist.

;;; Code:
(defconst machine-select-machine 'default
  \"This is the current machine that Emacs is running on.
The value of this constant may change the behavior of the
config.  Current valid values are 'laptop and 'desktop but others
may be added later.\")

(provide 'machine-select)
;;; machine-select.el ends here"))
  (when (not (file-exists-p machine-select-file))
    (message "machine-select.el does not exist, creating...")
    (with-temp-file machine-select-file
      (insert machine-select-contents))
    (message "machine-select.el created, but the value of machine-select-machine\
 is not yet valid. You should visit the file and change it to a valid value."))
  (require 'machine-select))

(require 'module-appearance)
(require 'module-git)
(require 'module-lang-general)
(require 'module-lang-java)
(require 'module-lang-c-c++)
(require 'module-lang-cl)
(require 'module-lang-csharp)
(require 'module-lang-haskell)
(require 'module-lang-scheme)
(require 'module-lang-web)
(require 'module-lang-python)
(require 'module-lang-arduino)
(require 'module-lang-go)
(require 'module-lang-ocaml)
(require 'module-lang-lua)
(require 'module-lang-coq)
(require 'module-lang-asm)
(require 'module-ledger)
(require 'module-pdf)
(require 'module-org)
(require 'module-chat)
(require 'module-email)
(require 'module-tex)
(require 'module-misc)

;; ----------------
;; Return GC threshold to normal
;; ----------------
(setq gc-cons-threshold 1000000)
