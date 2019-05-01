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
(use-package hl-line
  :hook
  (prog-mode . hl-line-mode))

;; Colours background under hex colour codes
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook prog-mode)

;; Colours words 
(use-package rainbow-identifiers
  :ensure t
  :custom
  (rainbow-identifiers-cie-l*a*b*-lightness 70)
  (rainbow-identifiers-cie-l*a*b*-saturation 20)
  (rainbow-identifiers-choose-face-function
   #'rainbow-identifiers-cie-l*a*b*-choose-face)
  :hook
  (emacs-lisp-mode . rainbow-identifiers-mode) ; actually, turns it off
  (prog-mode . rainbow-identifiers-mode))

(provide 'init-highlighting)

;;; init-highlighting.el ends here
