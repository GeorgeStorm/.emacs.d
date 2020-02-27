;;; init-magit.el --- File containing magit setup

;;; Commentary:
;;; Magit initialisation

;;; Code:

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-log-section-commit-count 40))

(provide 'init-magit)

;;; init-magit.el ends here
