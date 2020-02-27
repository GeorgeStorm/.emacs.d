;;; init-treemacs.el --- File browser

;;; Commentary:

;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  )

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(provide 'init-treemacs)

;;; init-treemacs.el ends here
