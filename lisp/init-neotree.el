;;; init-neotree.el --- File browser

;;; Commentary:

;;; Code:

(use-package neotree
  :defer t
  :ensure t
  :init
  (setq neo-window-width 50)
  :config
  (setq neo-smart-open t)
  )
(global-set-key [f8] 'neotree-toggle)
(provide 'init-neotree)

;;; init-neotree.el ends here
