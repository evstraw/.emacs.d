;; ---------------
;; Enable commands disabled by default
;; ---------------
(put 'downcase-region 'disabled nil)

;; ---------------
;; Generic LSP Configuration
;; ---------------
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :autoload (projectile-project-root))

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

(use-package minibuffer
  :demand t
  :bind (:map minibuffer-local-completion-map
         ("SPC" . nil)))

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
  (sp-with-modes '(java-mode c++-mode c-mode go-mode
                   groovy-mode arduino-mode rustic-mode rust-mode)
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

(global-hl-line-mode)
(column-number-mode)

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package gradle-mode
  :ensure t
  :commands (gradle-mode)
  :hook prog-mode)

(use-package simple
  :config (setq-default indent-tabs-mode nil
                        fill-column 85)
  :hook (prog-mode . auto-fill-mode))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package realgud
  :commands (realgud:gdb))

(use-package ansi-color
  :functions (ansi-color-apply-on-region)
  :config
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  :hook (compilation-filter . colorize-compilation-buffer))

(use-package flycheck
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  :hook (prog-mode . flycheck-mode))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s l" . consult-line)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "C-=")

  (setq consult-project-function (lambda (_) (projectile-project-root))))

(provide 'module-lang-general)
