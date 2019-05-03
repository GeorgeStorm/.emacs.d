;;; init-python.el --- Python config

;;; Commentary:
;; Loads elpy and Jedi for python compilation

;;; Code:

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq warning-suppress-types '((python)
                                 (emacs))))
  
(use-package elpy
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  (progn
    (setq elpy-rpc-backend "jedi")))

(provide 'init-python)

;;; init-python.el ends here
