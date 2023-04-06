(use-package hledger-mode
  ;;:custom (ledger-binary-path "hledger")
  ;;:custom (ledger-clear-whole-transactions t)
  :mode "\\.journal\\'"
  :hook '((hledger-mode . company-mode)
	  (hledger-mode . emojify-mode)))

(use-package flycheck-hledger
  :after (flycheck hledger-mode))


(provide 'module-ledger)
