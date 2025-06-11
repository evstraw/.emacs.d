;;; module-machine-config --- Defines machine-specific configuration options
;;; Commentary:

;;; Code:

(use-package paths
  :demand t
  :defines (path:machine-config-file
            path:user-home-directory))

(use-package f
  :ensure t
  :functions (f-expand)
  :autoload (f-expand))

(use-package initsplit
  :functions (initsplit-load)
  :defines (initsplit-customizations-alist)
  :ensure t
  :config
  (setq initsplit-customizations-alist
        `(("^machine:.*" ,path:machine-config-file t t)
          ("^default$" ,path:custom-file t t)))
  :hook (after-init . (lambda () (mapc #'initsplit-load initsplit-customizations-alist))))

(defgroup machine nil
  "Group for machine-specific settings."
  :group 'initialization)

(defcustom machine:org-directory (f-expand "Notes" path:user-home-directory)
  "Base directory for Org-Mode files containing notes on this machine."
  :type 'directory
  :group 'machine)

;;; module-machine-config.el ends here
(provide 'module-machine-config)
