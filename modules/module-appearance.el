;; -----------------
;; Configure themes
;; -----------------

(setq frame-resize-pixelwise t)

;; Display shell/eshell buffers in a new window instead of in the same window
(push '("\\`\\*e?shell" display-buffer-pop-up-window) display-buffer-alist)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 10)
                          (projects . 10))))

(use-package doom-themes
  :functions (doom-themes-visual-bell-config
	      doom-themes-org-config)
  :demand t
  :config
  (setq doom-themes-enable-bold t      ; If nil, bold is universally disabled
	doom-themes-enable-italic t)   ; If nil, italics is universally disabled
  (load-theme 'doom-outrun-electric t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package nerd-icons
  :after (doom-themes)
  :ensure t)

(use-package flymake
  :config (set-face-attribute 'flymake-warning nil :underline '(:color "yellow" :style wave))
  :defer t)

(use-package doom-modeline
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-height 24)
  (doom-modeline-mode)
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
