;; -----------------
;; Configure themes
;; -----------------

(setq frame-resize-pixelwise t)

(defvar dark-theme 'doom-outrun-electric "The theme to use when in dark mode.")
(defvar light-theme 'doom-acario-light "The theme to use when not in dark mode.")

(defcustom use-dark-mode t
  "Whether or not 'dark mode' should be activated, which uses a dark theme instead
of a light theme."
  :type '(boolean))

(declare-function doom-themes-init (buffer-file-name))

(defun dark-mode-on ()
  (interactive)
  (customize-save-variable 'use-dark-mode t)
  (doom-themes-init))

(defun dark-mode-off ()
  (interactive)
  (customize-save-variable 'use-dark-mode nil)
  (doom-themes-init))

(defun dark-mode-toggle ()
  (interactive)
  (if use-dark-mode
      (dark-mode-off)
    (dark-mode-on)))

;; Display shell/eshell buffers in a new window instead of in the same window
(push '("\\`\\*e?shell" display-buffer-pop-up-window) display-buffer-alist)

(use-package dashboard
  :init
  (global-unset-key (kbd "C-x C-n"))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 10)
                          (projects . 10)
                          (agenda . 7))))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t      ; If nil, bold is universally disabled
	doom-themes-enable-italic t)   ; If nil, italics is universally disabled
  (defun doom-themes-init ()
    (if use-dark-mode
	(load-theme dark-theme t)
      (load-theme light-theme t))
    (doom-themes-visual-bell-config)
    (doom-themes-org-config))
  (doom-themes-init))

(use-package nerd-icons
  :after (doom-themes)
  :ensure t)

(use-package flymake
  :config (set-face-attribute 'flymake-warning nil :underline '(:color "yellow" :style wave))
  :defer t)

(use-package doom-modeline
  :init
  (defun doom-modeline-daemon (&optional frame)
    "`doom-modeline' strangely does not behave in a daemon-session.
So we need to fix this ourselves."
    (setq doom-modeline-icon t)
    (setq doom-modeline-height 30))
  (add-hook 'after-make-frame-functions #'doom-modeline-daemon)
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-mode)
  (doom-modeline-daemon)
  (doom-themes-visual-bell-config)
  :after (doom-themes nerd-icons))

(use-package org
  :commands (org-agenda)
  :config
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil))

;; allow splitting narrower windows
(setq split-width-threshold 154)

;; fix font
(require 'machine-select)
(set-face-attribute 'default nil
		    :family "DejaVu Sans Mono"
		    :height (if (eq machine-select-machine 'desktop) 94 95)
		    :width 'normal)

(use-package emojify
  :commands (emojify-mode))

(provide 'module-appearance)
