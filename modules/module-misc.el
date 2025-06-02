(use-package csv-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package graphviz-dot-mode
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

(use-package transient
  :init
  (defun find-code-dir ()
    (interactive)
    (find-file "~/Sync/code/"))
  (defun find-school-dir ()
    (interactive)
    (find-file "~/Documents/school/"))
  (defun find-notes-dir ()
    (interactive)
    (find-file "~/Documents/notes/"))
  (defun find-robotics-dir ()
	(interactive)
	(find-file "~/Sync/code/robotics/"))
  (defun find-guix-dir ()
    (interactive)
    (find-file "~/Sync/code/guix-channel/"))
  (defun find-init-module ()
    (interactive)
    (let ((default-directory "~/.emacs.d/modules/"))
      (call-interactively 'find-file)))
  (defun find-manifest-file ()
    (interactive)
    (let ((default-directory "~/Sync/code/manifest/"))
      (call-interactively 'find-file)))
  (defun find-dotfiles ()
    (interactive)
    (let ((default-directory "~/Sync/code/dotfiles/"))
      (call-interactively 'find-file)))
  (defun visit-file-truename ()
    (interactive)
    (if (eq major-mode 'dired-mode)
	(find-file (file-truename default-directory))
      (if (buffer-file-name)
	  (find-file (file-truename (buffer-file-name)))
	(message "Buffer is not visiting a file!"))))
  :config
  (transient-define-prefix quick-goto ()
    "Quickly runs a command from a popup window."
    ["Common directories and files"
     ("c" "Open code directory" find-code-dir)
     ("s" "Open school directory" find-school-dir)
	 ("r" "Open robotics directory" find-robotics-dir)
     ("n" "Open notes directory" find-notes-dir)
     ("g" "Open guix channel" find-guix-dir)]
    ["Find configuration files"
     ("i" "Open init file" open-init-file)
     ("m" "Open init module" find-init-module)
     ("d" "Open dotfile" find-dotfiles)
     ("p" "Open Guix manifest file" find-manifest-file)]
    ["Common commands"
     ("a" "Org Agenda" org-agenda)
     ("M" "Mail" gnus)
     ("f" "Visit linked file" visit-file-truename)])
  :bind ("C-<menu>" . quick-goto)
  ("C-<f12>" . quick-goto))

(setq enable-remote-dir-locals t)

(use-package tramp
  :config
  (let ((process-environment tramp-remote-process-environment))
    (setenv "ENV" "$HOME/.profile")
    (setq tramp-remote-process-environment process-environment)))

(global-set-key (kbd "C-x <f5>") #'revert-buffer)

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
  :commands (esup))
  ;;*  :config (setq esup-depth 0))
  
(provide 'module-misc)
