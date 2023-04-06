;; ---------------
;; LSP Java Configuration
;; ---------------
(use-package lsp-java
  :commands (lsp-java-lens-mode)
  :hook ((java-mode . lsp))
  :config
  (setq lsp-java-vmargs
	'(;;"-noverify"
	  "-Xmx1G"
	  "-XX:+UseG1GC"
	  "-XX:+UseStringDeduplication"))
  (setq lsp-java-import-gradle-enabled nil)
  (setq tab-width 4)
  (use-package lsp-treemacs)
  :bind (:map java-mode-map
	      ("C-c C-g" . gradle-run)
	      ("C-c C-r" . lsp-rename)
	      ("C-c RET" . lsp-execute-code-action)
	      ("C-c C-f" . lsp-format-buffer)))

(use-package jflex-mode
  :load-path thirdparty-dir
  :mode "\\.jflex\\'")

(use-package flycheck
  :commands (flycheck-mode)
  :hook (java-mode . flycheck-mode))

(use-package dap-mode :after lsp-mode
  :commands (dap-mode)
  :hook (java-mode . dap-mode)
  :config (dap-auto-configure-mode))

(provide 'module-lang-java)
