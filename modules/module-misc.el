(use-package f
  :ensure t
  :autoload (f-expand))

(use-package csv-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package graphviz-dot-mode
  :defines (graphviz-dot-indent-width)
  :config
  (setq graphviz-dot-indent-width 4)
  :commands (graphviz-dot-mode)
  :defer t)

(use-package company-graphviz-dot
  :defer t)

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package debbugs
  :commands (debbugs-gnu))

(use-package disk-usage
  :commands (disk-usage disk-usage-here))

(use-package ace-window
  :commands (ace-window
	     ace-window-display-mode)
  :config
  (setq aw-dispatch-always t
	aw-display-mode-overlay nil
	aw-background nil)
  :bind ("C-x C-o" . ace-window)
  :hook (emacs-startup . ace-window-display-mode))

(use-package dired
  :config (setq dired-dwim-target t))

(defun open-init-file ()
  "Opens the init.el file quickly."
  (interactive)
  (find-file (f-expand "init.el" user-emacs-directory)))

(use-package module-machine-config
  :defines (machine:org-directory))

(defun visit-file-truename ()
  "Attempt to visit the link target of the current buffer's visited file.

If the current buffer is visiting a file or directory, attempts
  to resolve symbolic links to find the \"true\" file path (by
  visiting the file or directory given by `file-truename').

This can be useful in situations where you have a symbolically
  linked file and are attempting to use tools or minor modes that
  do not play well with symbolic links (e.g. some LSP servers)."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (find-file (file-truename default-directory))
    (if (buffer-file-name)
	(find-file (file-truename (buffer-file-name)))
      (message "Buffer is not visiting a file!"))))

(use-package transient
  :init
  (defun find-code-dir ()
    (interactive)
    (find-file "~/Sync/code/"))
  (defun find-notes-dir ()
    (interactive)
    (find-file machine:org-directory))
  (defun find-guix-dir ()
    (interactive)
    (find-file "~/Sync/code/guix-channel/"))
  (defun find-init-module ()
    (interactive)
    (let ((default-directory "~/.emacs.d/modules/"))
      (call-interactively 'find-file)))
  (defun find-dotfiles ()
    (interactive)
    (let ((default-directory "~/Sync/code/dotfiles/"))
      (call-interactively 'find-file)))
  :config
  (transient-define-prefix quick-goto ()
    "Quickly runs a command from a popup window."
    ["Common directories and files"
     ("c" "Open code directory" find-code-dir)
     ("n" "Open notes directory" find-notes-dir)
     ("g" "Open guix channel" find-guix-dir)]
    ["Find configuration files"
     ("i" "Open init file" open-init-file)
     ("m" "Open init module" find-init-module)
     ("d" "Open dotfile" find-dotfiles)]
    ["Common commands"
     ("a" "Org Agenda" org-agenda)
     ("f" "Visit linked file" visit-file-truename)])
  :bind (("C-<menu>" . quick-goto)
	 ("C-<f12>" . quick-goto)
	 ("C-<XF86Tools>" . quick-goto)))

(setq enable-remote-dir-locals t)

(use-package tramp
  :config
  (let ((process-environment tramp-remote-process-environment))
    (setenv "ENV" "$HOME/.profile")
    (setq tramp-remote-process-environment process-environment)))

(global-set-key (kbd "C-x <f5>") #'revert-buffer)
(global-set-key (kbd "s-u") #'revert-buffer)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x 6") #'toggle-window-split)

(use-package esup
  :commands (esup)
  :config
  ;; Fixes a bug that occurs when trying to fontify results, see
  ;; https://www.reddit.com/r/emacs/comments/13jh7gk/comment/jkf225g/
  (setq esup-depth 0))
  
(provide 'module-misc)
