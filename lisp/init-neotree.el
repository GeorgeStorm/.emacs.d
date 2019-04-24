;;; init-neotree.el --- File browser

;;; Commentary:

;;; Code:

(use-package neotree
  :init
  (require 'neotree)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  )
(global-set-key [f8] 'neotree-toggle)
(provide 'init-neotree)

;;; init-neotree.el ends here
