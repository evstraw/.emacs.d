;; ----------
;; OCaml Configuration
;; ----------

(use-package tuareg
  :commands (tuareg-mode)
  :mode (("\\.ml[ily]?$" . tuareg-mode)
	 ("\\.topml$" . tuareg-mode)))

(use-package merlin
  :commands (merlin-mode)
  :after tuareg
  :custom
  (merlin-error-after-save nil "Don't check for errors on saving")
  :hook (tuareg-mode . merlin-mode))

(use-package flycheck-ocaml
  :after (merlin tuareg)
  :config (message "Flycheck ocaml loaded")
  :commands (flycheck-ocaml-setup)
  :hook (tuareg-mode . flycheck-ocaml-setup))

(use-package ocp-indent
  :if (executable-find "ocp-indent")
  :commands (ocp-indent-buffer))

(use-package ocamlformat
  :commands (ocamlformat)
  :custom (ocamlformat-enable 'enable-outside-detected-project))

(use-package utop
  :commands (utop
	     utop-minor-mode)
  :hook ((tuareg-mode . utop-minor-mode)
	 (utop-mode . company-mode)))

(use-package dune
  :commands (dune-mode)
  :mode ("dune" . dune-mode))

(provide 'module-lang-ocaml)
