(use-package ein
  :mode ("\\.ipynb\\'" . ein:ipynb-mode)
  :commands (ein:run)
  :custom (ein:output-area-inlined-images t))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config (setq python-shell-interpreter "python3"))

;;(setq python-shell-interpreter "/usr/bin/python3")

(use-package pyvenv
  :commands (pyvenv-activate
	     pyvenv-workon))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;;(add-hook 'ein:notebook-mode-hook
;;	  (lambda ()
;;	    (auto-fill-mode -1)
;;	    (local-set-key (kbd "C-c C-d") (ein:worksheet-delete-cell))))

(provide 'module-lang-python)
