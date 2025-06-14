;; -*- lexical-binding: t; -*-

;; --------------
;; Disable all mouse-interactive interfaces early.
;; --------------
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;*  Local Variables:
;;*  flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;;*  End:
