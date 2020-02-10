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

(provide 'init-python)

;;; init-python.el ends here
