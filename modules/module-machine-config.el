;;; module-machine-config --- Defines machine-specific configuration options
;;; Commentary:

;;; Code:

(use-package paths
  :demand t
  :defines (path:machine-config-file
            path:user-home-directory))

(use-package f
  :ensure t
  :autoload (f-expand))

(use-package initsplit
  :ensure t
  :demand t
  :init (defalias 'find-if 'cl-find-if)
  (setq custom-file path:custom-file
        initsplit-customizations-alist
        `(("^machine:.*" ,path:machine-config-file t t)))
  (load custom-file))

(defgroup machine nil
  "Group for machine-specific settings."
  :group 'initialization)

(defcustom machine:org-directory (f-expand "Notes" path:user-home-directory)
  "Base directory for Org-Mode files containing notes on this machine."
  :type 'directory
  :group 'machine)

(defcustom machine:org-roam-directory machine:org-directory
  "Base directory for Org-Roam files on this machine."
  :type 'directory
  :group 'machine)

(defcustom machine:org-roam-exclude '("logseq/")
  "List of directory or file paths to be excluded from Org-Roam on this machine.

Paths should be relative to `machine:org-roam-directory'."
  :type '(repeat string)
  :group 'machine
  :set-after '(machine:org-directory))

;;; module-machine-config.el ends here
(provide 'module-machine-config)
