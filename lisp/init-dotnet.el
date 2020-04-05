;;; init-dotnet.el --- File containing .NET setup

;;; Commentary:
;;; Initialisation for .NET editing

;;; Code:

;;; Setup to match Visual Studio (tab-width 3)
(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode))

(use-package omnisharp
  :defer t
  :init
  (setq omnisharp-server-executable-path "~/.emacs.d/.cache/omnisharp/server/v1.32.18/OmniSharp.exe")
  :after company
  :config
  (add-to-list 'company-backends 'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 3)
  (setq truncate-lines t)
  (setq tab-width 3)

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(provide 'init-dotnet)

;;; init-dotnet.el ends here
