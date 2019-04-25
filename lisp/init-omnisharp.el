;;; init-omnisharp.el --- File containing omnisharp setup

;;; Commentary:
;;; Omnisharp initialisation

;;; Code:

(use-package omnisharp
  :defer t
  :init
  (setq omnisharp-server-executable-path "~/.emacs.d/.cache/omnisharp/server/v1.32.18/OmniSharp.exe")
  :after company
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  (add-hook 'csharp-mode-hook 'omnisharp-mode))
(provide 'init-omnisharp)

;;; init-omnisharp.el ends here
