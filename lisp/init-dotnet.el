;;; init-dotnet.el --- File containing .NET setup

;;; Commentary:
;;; Initialisation for .NET editing

;;; Code:

;;; Setup to match Visual Studio (tab-width 3)
(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode)
  :config
  (setq indent-tabs-mode nil
	c-syntactic-indentation t
	c-basic-offset 3
	truncate-lines t
	tab-width 3))

(use-package omnisharp
  :defer t
  :init
  (setq omnisharp-server-executable-path "~/.emacs.d/.cache/omnisharp/server/v1.32.18/OmniSharp.exe")
  :after company
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  (add-hook 'csharp-mode-hook 'omnisharp-mode))
(provide 'init-dotnet)

;;; init-dotnet.el ends here
