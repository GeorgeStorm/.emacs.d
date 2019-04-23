;;; init-omnisharp.el --- File containing omnisharp setup

;;; Commentary:
;;; Omnisharp initialisation

;;; Code:

(use-package omnisharp
  :after company
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp))
(setq omnisharp-server-executable-path "~/.emacs.d/.cache/omnisharp/server/v1.32.18/OmniSharp.exe")
(provide 'init-omnisharp)

;;; init-omnisharp.el ends here
