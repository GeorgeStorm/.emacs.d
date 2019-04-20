;;; lisp/init-company.el

;;; Code:
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-idle-delay 0)

  :config
  (add-hook 'after-init-hook 'global-company-mode))
(provide 'init-company)
