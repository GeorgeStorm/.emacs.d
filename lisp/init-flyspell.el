(use-package flyspell
  :init
  :config
  (progn 
    (setq ispell-program-name "hunspell")
    (setq ispell-list-command "--list") ;; run flyspell with aspell, not ispell
    
  (add-hook 'text-mode-hook #'turn-on-flyspell)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)))
(flyspell-mode 1)
(provide 'init-flyspell)
