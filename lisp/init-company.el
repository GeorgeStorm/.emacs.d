;;; init-company.el --- Company for autocompletion

;;; Commentary:

;;; Code:
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-idle-delay 0
        company-minimum-prefix-length 1)
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(setq company-dabbrev-downcase nil)

(provide 'init-company)

;;; init-company.el ends here
