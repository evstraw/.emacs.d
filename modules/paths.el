;; -*- lexical-binding: t -*-
;;; paths --- Various useful file/directory paths for Emacs config

;;; Commentary:
;;; Defines a few useful file/directory paths for other parts of the Emacs configuration.
;;; To avoid naming collisions, all symbols are prefixed with `path:'.

;;; Code:

(use-package f
  :autoload (f-expand))

(defconst path:user-home-directory (getenv "HOME")
  "The user's home directory (~).")

(defconst path:modules-dir (f-expand "modules" user-emacs-directory))
(defconst path:thirdparty-dir (f-expand "thirdparty" user-emacs-directory))

(defconst path:custom-file (f-expand "custom.el" user-emacs-directory))
(defconst path:machine-config-file (f-expand "machine-config.el" user-emacs-directory))

(provide 'paths)
;;; paths.el ends here
