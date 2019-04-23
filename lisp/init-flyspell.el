;;; init-flyspell.el --- File containing flyspell setup

;;; Commentary:
;;; Flycheck initialisation

;;; Code:

(use-package flyspell
  :config
  (progn 
    (setq ispell-program-name "hunspell")
    (setq ispell-list-command "--list") ;; run flyspell with aspell, not ispell
    (setq flyspell-issue-message-flag nil)
    
  (add-hook 'text-mode-hook #'turn-on-flyspell)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)))
(provide 'init-flyspell)

;;; init-flyspell.el ends here
