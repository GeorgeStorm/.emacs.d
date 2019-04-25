;;; init-company.el --- Company for autocompletion

;;; Commentary:

;;; Code:
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-idle-delay 0.1)

  :config
  (add-hook 'after-init-hook 'global-company-mode))
(provide 'init-company)

;;; init-company.el ends here
