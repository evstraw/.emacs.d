;;; module-machine-config --- Defines machine-specific configuration options
;;; Commentary:

;;; Code:

(use-package paths
  :demand t
  :defines (path:machine-config-file))

(use-package initsplit
  :functions (initsplit-load)
  :demand t
  :config
  (setq initsplit-customizations-alist
	`(("^machine:.*" ,path:machine-config-file
	   "^default$" ,path:machine-config-file))))

(defgroup machine nil
  "Group for machine-specific settings."
  :group 'initialization)

(mapc #'initsplit-load initsplit-customizations-alist)

;;; module-machine-config.el ends here
(provide 'module-machine-config)
