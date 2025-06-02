;; ---------------
;; Enable commands disabled by default
;; ---------------
(put 'downcase-region 'disabled nil)

;; ---------------
;; Generic LSP Configuration
;; ---------------
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package neotree
  :commands (neotree))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "M-l")
  :commands (lsp)
  :bind (:map lsp-mode-map
			  ("C-c C-r" . lsp-rename))
  :config
  (setq lsp-file-watch-threshold 3000)
  (setq read-process-output-max (* 1024 1024))
  :hook ((lsp-mode . lsp-enable-which-key-integration)))

(use-package which-key
  :config
  (which-key-mode))

(use-package lsp-ui
  :commands (lsp-ui-mode))

(use-package yasnippet
  :config
  (yas-reload-all)
  (defun yas-commit-editmsg ()
    (when (string= (buffer-name) "COMMIT_EDITMSG")
      (yas-minor-mode-on)))
  :commands (yas-minor-mode)
  :hook ((prog-mode . yas-minor-mode)
	 (text-mode . yas-commit-editmsg)))

(use-package company
  :commands (company-mode)
  :config
  (setq company-idle-delay 0.1)
  (delete 'company-clang company-backends)
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
	      ("M-n" . nil)
	      ("M-p" . nil)
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)))

(use-package company-quickhelp
  :after (company)
  :hook (prog-mode . company-quickhelp-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (sp-with-modes '(java-mode c++-mode c-mode go-mode groovy-mode arduino-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
     					       (" ||\n[i]" "RET")))
    (sp-local-pair "'" "'"))
  (sp-with-modes '(css-mode js2-mode js-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
     					       (" ||\n[i]" "RET"))))
  (sp-with-modes '(tuareg-mode utop-mode)
    (sp-local-pair "(*" "*)" :post-handlers '(("| " "SPC")
					      (" ||\n[i]" "RET")))))

;; ---------------
;; Generic configuration for all languages
;; ---------------

;;*  (use-package column-enforce-mode
;;*    :commands (column-enforce-mode)
;;*    :ensure t
;;*    :config
;;*    (setq column-enforce-column 95)
;;*    :hook prog-mode)

(global-hl-line-mode)
(column-number-mode)

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package gradle-mode
  :commands (gradle-mode)
  :hook prog-mode)

(add-hook 'prog-mode-hook
	  #'(lambda ()
	      (display-line-numbers-mode)
	      (auto-fill-mode)
	      (setq fill-column 95)
	      (display-fill-column-indicator-mode)))

(use-package realgud
  :commands (realgud:gdb))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(provide 'module-lang-general)
