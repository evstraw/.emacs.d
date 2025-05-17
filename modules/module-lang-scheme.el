;; ---------------
;; Scheme config
;; ---------------


(defvar guix-checkout "~/Sync/code/guix"
  "The location of the local Guix checkout.")

(defvar guix-load-path "~/.config/guix/current/share/guile/site/3.0"
  "The location of Guix Scheme modules.")

(defun setup-scheme-style ()
    (put 'eval-when 'scheme-indent-function 1)
    (put 'call-with-prompt 'scheme-indent-function 1)
    (put 'test-assert 'scheme-indent-function 1)
    (put 'test-assertm 'scheme-indent-function 1)
    (put 'test-equalm 'scheme-indent-function 1)
    (put 'test-equal 'scheme-indent-function 1)
    (put 'test-eq 'scheme-indent-function 1)
    (put 'call-with-input-string 'scheme-indent-function 1)
    (put 'guard 'scheme-indent-function 1)
    (put 'lambda* 'scheme-indent-function 1)
    (put 'substitute* 'scheme-indent-function 1)
    (put 'sxml-match 'scheme-indent-function 1)
    (put 'pre-post-order 'scheme-indent-function 1)
    (put 'match-record 'scheme-indent-function 2))
(use-package scheme
  :config
  :hook ((scheme-mode . setup-scheme-style)))

(use-package geiser
  :commands (geiser
	     run-geiser)
  :custom (geiser-default-implementation 'guile)
  :init
  (put 'geiser-guile-load-path 'safe-local-variable #'listp)
  (setq geiser-active-implementations '(guile racket chicken))
  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path guix-checkout)
    (add-to-list 'geiser-guile-load-path guix-load-path))
  :config
  :hook ((scheme-mode . (lambda ()
			  (geiser-mode)
			  (rainbow-delimiters-mode)
			  (setup-prettify-symbols)))
	 (geiser-repl-mode . (lambda ()
			       (rainbow-delimiters-mode)
			       (setup-scheme-style)
			       (setup-prettify-symbols)))))

(use-package yasnippet
  :commands (yas-minor-mode)
  :config
  (add-to-list 'yas-snippet-dirs (concat guix-checkout "/etc/snippets")))

(use-package racket-mode
  :mode ("\\.rkt\\'")
  :bind (:map racket-mode-map
	      ("C-c C-." . racket-xp-describe))
  :hook (((racket-mode
	   racket-repl-mode) . (lambda ()
				 (rainbow-delimiters-mode)
				 (company-mode)
				 (setup-prettify-symbols)))))

(defun setup-prettify-symbols ()
  (setq prettify-symbols-alist '(("lambda" . 955)
				 ("->" . 8594)))
  (prettify-symbols-mode))

(use-package scribble-mode
  :mode ("\\.scrbl\\'"))

(provide 'module-lang-scheme)
