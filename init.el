;; -*- lexical-binding: t; -*-

;; ----------------
;; Temporarily raise GC threshold while loading
;; ----------------
(setq gc-cons-threshold most-positive-fixnum)

;; ----------------
;; Configure package sources
;; ----------------
(eval-when-compile
  (require 'use-package))

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize t))

;; ---------------
;; Load some libraries needed for other parts of config
;; ---------------
(use-package paths
  :demand t
  :load-path "modules"
  :defines (path:modules-dir
	    path:custom-file))

(require 'module-machine-config)

;; ---------------
;; Save backup and autosave files to /tmp.
;; ---------------
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'module-appearance)
(require 'module-git)
(require 'module-lang-general)
(require 'module-lang-java)
(require 'module-lang-c-c++)
(require 'module-lang-cl)
(require 'module-lang-csharp)
(require 'module-lang-scheme)
(require 'module-lang-web)
(require 'module-lang-python)
(require 'module-lang-arduino)
(require 'module-lang-go)
(require 'module-lang-ocaml)
(require 'module-lang-lua)
(require 'module-lang-coq)
(require 'module-lang-asm)
(require 'module-lang-rust)
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

;;*  Local Variables:
;;*  flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;;*  End:
