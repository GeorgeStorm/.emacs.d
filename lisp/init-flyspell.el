;;; init-flyspell.el --- File containing flyspell setup

;;; Commentary:
;;; Flycheck initialisation

;;; Code:

(use-package ispell
  :defer t
  :custom
  (ispell-program-name "hunspell")
  (ispell-program-name "c:/hunspell/bin/hunspell.exe")
  (ispell-dictionary "en_GB")
  (ispell-really-aspell nil)
  (ispell-really-hunspell t)
  (ispell-encoding8-command t)
  (ispell-silently-savep t))

(use-package flyspell
  :defer t
  :custom
  (flyspell-delay 1)
  (add-hook 'text-mode-hook #'turn-on-flyspell)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(provide 'init-flyspell)

;;; init-flyspell.el ends here
