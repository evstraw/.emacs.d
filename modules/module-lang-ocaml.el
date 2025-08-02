;; ----------
;; OCaml Configuration
;; ----------

(use-package tuareg
  :commands (tuareg-mode)
  :mode (("\\.ml[ily]?$" . tuareg-mode)
	 ("\\.topml$" . tuareg-mode)))

(use-package flycheck-ocaml
  :after merlin
  :config (message "Flycheck ocaml loaded")
  :commands (flycheck-ocaml-setup)
  :hook (tuareg-mode . flycheck-ocaml-setup))

(use-package merlin
  :load-path "~/.opam/default/share/emacs/site-lisp"
  :commands (merlin-mode)
  :custom
  (merlin-error-after-save nil "Don't check for errors on saving")
  :hook (tuareg-mode . merlin-mode))

(use-package ocp-indent
  :commands (ocp-indent-buffer)
  :ensure t)

(use-package ocamlformat
  :commands (ocamlformat)
  :load-path "~/.opam/default/share/emacs/site-lisp"
  :custom (ocamlformat-enable 'enable-outside-detected-project))

(use-package utop
  :ensure t
  :commands (utop
	     utop-minor-mode)
  :after (company)
  :hook ((tuareg-mode . utop-minor-mode)
	 (utop-mode . company-mode)))

(use-package dune
  :commands (dune-mode)
  :mode ("dune" . dune-mode))

(provide 'module-lang-ocaml)
