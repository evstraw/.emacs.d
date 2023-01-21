(use-package haskell-mode)
(use-package lsp-haskell
  :after (lsp-mode haskell-mode)
  :hook ((haskell-mode . lsp)))

(provide 'module-lang-haskell)
