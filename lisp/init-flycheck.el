;;; lisp/init-flycheck.el

;;; Code:
(use-package flycheck
  :ensure t
  :config (add-hook 'prog-mode-hook 'flycheck-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)
(provide 'init-flycheck)
