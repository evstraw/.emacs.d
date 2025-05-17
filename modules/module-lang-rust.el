(use-package rustic
  :custom (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu))
  :hook ((rustic-mode . lsp)))

(provide 'module-lang-rust)
