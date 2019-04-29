;;; init-org.el --- Orgmode setup

;;; Commentary:

;;; Code:

(setq org-src-tab-acts-natively t)
(add-hook 'org-mode-hook #'toggle-word-wrap)
(setq org-startup-truncated nil)

(provide 'init-org)

;;; init-org.el ends here
