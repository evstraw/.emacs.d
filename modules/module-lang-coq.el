(defun setup-pg-fonts ()
  (set-face-attribute 'proof-active-area-face nil :background "#1b1834")
  (set-face-attribute 'proof-locked-face nil :background "#1b1834"))

(use-package proof-general
  :commands (proofgeneral)
  :custom ((coq-compile-before-require t))
  :hook ((coq-mode . proofgeneral)
	 (coq-mode . setup-pg-fonts)))

(use-package company-math)

(use-package company-coq
  :commands (company-coq-mode)
  :hook ((coq-mode . company-coq-mode)
	 (company-coq-mode . (lambda () (company-quickhelp-mode -1)))))

(provide 'module-lang-coq)
