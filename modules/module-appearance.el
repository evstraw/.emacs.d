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
  :after (doom-themes))

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

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :hook (minibuffer-mode . marginalia-mode))

(provide 'module-appearance)
