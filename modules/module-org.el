;; -*- lexical-binding: t; -*-
(use-package f
  :ensure t
  :autoload (f-ancestor-of-p
             f-expand
             f-relative))

(use-package module-machine-config
  :defines (machine:org-directory))

(use-package recentf
  :defines (recentf-exclude))

(use-package org-agenda
  :commands (org-agenda
             org-agenda-redo-all
             org-agenda-revert-all
             org-agenda-redo-or-revert)
  :functions (org-get-agenda-file-buffers
              my/org-agenda-list-exclude-tags-advice)
  :bind ( :map org-agenda-mode-map
          ("g" . org-agenda-redo-or-revert))
  :config
  (defun org-get-agenda-file-buffers ()
    "Return all open agenda file buffers."
    (mapcar #'org-get-agenda-file-buffer org-agenda-files))

  (defun org-agenda-revert-all ()
    "Reverts all Org buffers used for the Org agenda, reloading them from disk."
    (interactive)
    (mapcar (lambda (buf)
              (with-current-buffer buf
                (revert-buffer t t)))
            (org-get-agenda-file-buffers)))

  (defun org-agenda-redo-or-revert (&optional revert)
    "Rebuild all agenda views in the current buffer.
With a prefix argument (REVERT), revert all agenda buffers before
doing so."
    (interactive "P")
    (when revert
      (org-agenda-revert-all))
    (org-agenda-redo-all))

  (defun my/org-agenda-list-exclude-tags-advice (orig-fn &rest args)
    "Exclude selected tags from `org-agenda-list'.
Intended as :around advice for `org-agenda-list'."
    (let ((org-agenda-tag-filter-preset '("-noagenda")))
      (apply orig-fn args)))
  (advice-add #'org-agenda-list :around #'my/org-agenda-list-exclude-tags-advice)
  
  (setq org-agenda-start-on-weekday 1
        org-deadline-warning-days 6
        org-columns-default-format "%ITEM %TODO %3PRIORITY %CLOCKSUM(Time) %Effort{:} %TAGS"))

(use-package org
  :functions (org-get-agenda-file-buffer
              org-set-emph-re
              org-agenda-files)
  :commands (org-id-get-create)
  :config
  (add-to-list 'org-export-backends 'md)
  (setq org-directory machine:org-directory
        org-default-notes-file (expand-file-name "general.org" org-directory))
  (let ((scale 1.5))
    (setq org-format-latex-options
          (plist-put (plist-put org-format-latex-options :html-scale scale) :scale scale)))
  ;; Allow multiple line Org emphasis markup.
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))
  :hook ((org-mode . auto-fill-mode)
         (org-mode . org-indent-mode)
         (org-mode . company-mode)))

(use-package org-habit
  :after org
  :config
  (setq org-habit-graph-column 55
        org-habit-show-habits-only-for-today nil))

(use-package org-clock
  :after org
  :functions (org-clocking-p)
  :commands (org-clock-out
             org-clock-in-last)
  :config
  (defun my/org-clock-toggle ()
    (interactive)
    (if (org-clocking-p)
        (call-interactively #'org-clock-out)
      (call-interactively #'org-clock-in-last)))
  :bind (("S-<f15>" . my/org-clock-toggle)))

(use-package org-capture
  :after org
  :bind (("C-x c" . org-capture))
  :config
  (setq org-capture-templates
        '(("n" "Quick Note" entry
           (file+headline org-default-notes-file "General Notes")
           "* Note from %T\n %?%i\n  %a" :empty-lines 1 :kill-buffer t)
          ("t" "Quick todo" entry
           (file+olp org-default-notes-file "General Agenda" "Other")
           "* TODO [#B] %?\n %i\n " :empty-lines 1 :kill-buffer t)
          ("s" "Shopping list item" item
           (file+olp org-default-notes-file "Shopping List" "Unspecified Store")
           "- %?%i"))))

(use-package org-roam-dailies
  :after org-roam
  :commands (org-roam-dailies-capture-today
             org-roam-dailies-goto-today
             org-roam-dailies-goto-date
             org-roam-dailies-goto-yesterday)
  :defines (org-roam-dailies-directory
            org-roam-dailies-capture-templates))

(use-package org-roam-db
  :after org-roam
  :commands (org-roam-db-autosync-mode))

(use-package org-roam
  :functions (my/org-roam-file-list
              my/org-roam-refresh-agenda-list
              org-roam-node-list
              org-roam-node-file
              my/org-roam-include-node-at-point-p)
  :bind* ( :prefix-map my/org-roam-quick-map
           :prefix "C-x C-n"
           ("f" . org-roam-node-find)
           ("i" . org-roam-node-insert)
           ("b" . org-roam-buffer-toggle)
           ("t" . org-roam-dailies-capture-today)
           ("T" . org-roam-dailies-goto-today)
           ("Y" . org-roam-dailies-goto-yesterday)
           ("D" . org-roam-dailies-goto-date)
           ("u" . org-id-get-create))
  :config
  (setq org-roam-directory machine:org-roam-directory)

  (defun my/org-roam-include-node-at-point-p ()
    "Determine whether Org node at point should be included in the database."
    (when-let ((bfilename (buffer-file-name))
               (fname (f-relative bfilename org-roam-directory)))
      (not (cl-find-if (lambda (path) (f-ancestor-of-p path fname))
                       machine:org-roam-exclude))))

  (setq org-roam-db-node-include-function #'my/org-roam-include-node-at-point-p
        org-roam-completion-everywhere t
        org-roam-dailies-directory "journals/")
  (let ((daily-target '(file+head "%<%Y_%m_%d>.org" "#+title: %<%b %-d, %Y>\n"))
        (page-target '(file+head "pages/${slug}.org" "#+title: ${title}\n")))
    (setq org-roam-capture-templates
          `(("d" "default" plain
             "%?"
             :target ,page-target
             :unnarrowed t))
          org-roam-dailies-capture-templates
          `(("d" "default" entry
             "* %?"
             :target ,daily-target)
            ("t" "task" entry
             "* TODO %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:"
             :target ,daily-target))))
  (defun my/org-roam-file-list ()
    "Returns a list of files containing nodes in the Org-Roam database."
    (seq-uniq (mapcar #'org-roam-node-file (org-roam-node-list))))
  (defun my/org-roam-refresh-agenda-list ()
    "Refreshes the org agenda files list with all files being tracked by Org-Roam."
    (interactive)
    (setq org-agenda-files (my/org-roam-file-list)))

  ;; Build the agenda list the first time for the session
  (my/org-roam-refresh-agenda-list)

  (org-roam-db-autosync-mode))

(provide 'module-org)
