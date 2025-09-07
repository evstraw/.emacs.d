;; ---------------
;; Scheme config
;; ---------------

(use-package geiser
  :commands (geiser)
  :defines (geiser-active-implementations
            geiser-default-implementation)
  :config
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
  :custom (( geiser-active-implementations '(guile racket)
             "Use Geiser for Guile and Racket")
           ( geiser-implementations-alist
             '(((regexp "guix") guile)
               ((regexp "\\.scm$") guile)
               ((regexp "\\.rkt$") racket))
             "Use Guile for Guix and .scm files in general, Racket for .rkt files")
           ( geiser-default-implementation 'guile
             "Use Guile as default Scheme implementation"))
  :hook ((scheme-mode . (lambda ()
			  (geiser-mode)
			  (rainbow-delimiters-mode)
			  (setup-prettify-symbols)))
	 (geiser-repl-mode . (lambda ()
			       (rainbow-delimiters-mode)
			       (setup-scheme-style)
			       (setup-prettify-symbols)))))

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
