;;; init-flycheck.el --- Error checking

;;; Commentary:

;;; Code:

(use-package flycheck
  :defer t
  :ensure t
  :config (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
