;;; init-python.el --- Python config

;;; Commentary:
;; Loads elpy and Jedi for python compilation

;;; Code:

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)

  (require 'flycheck-pyflakes)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint)

  (setq warning-suppress-types '((python)
                                 (emacs))))
  
(use-package elpy
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  (progn
    (setq elpy-rpc-backend "jedi")))
(provide 'init-python)
;;; init-python.el ends here
