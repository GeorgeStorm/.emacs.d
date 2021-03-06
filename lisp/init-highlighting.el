;;; init-highlighting.el --- General file highlighting rules

;;; Commentary:

;;; Code:

;; Highlights word under cursor
(use-package idle-highlight-mode
  :ensure t
  :hook prog-mode)

;; Highlights matching parens
(use-package smartparens
  :defer 1
  :delight
  :custom (sp-escape-quotes-after-insert nil)
  :config (smartparens-global-mode 1))

;; Colours paren pairs
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlights numbers
(use-package highlight-numbers
  :ensure t
  :hook
  (prog-mode . highlight-numbers-mode))

;; Highlights escape sequences
(use-package highlight-escape-sequences
  :ensure t
  :config (hes-mode))

;; Highlights current line
(use-package hl-line)
(global-hl-line-mode 1)
;; Colours background under hex colour codes
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook prog-mode)

;; Remove trailing whitespace on file savex
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(provide 'init-highlighting)

;;; init-highlighting.el ends here
