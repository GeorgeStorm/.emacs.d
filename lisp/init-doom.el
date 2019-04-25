;;; init-doom.el --- Doom modeline and theme

;;; Commentary:

;;; Code:

(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-init)
  :custom
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon t))
	  
(use-package doom-themes
	:config

	;; Global settings (defaults)
	(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
		  doom-themes-enable-italic t) ; if nil, italics is universally disabled

	;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
	;; may have their own settings.
	(load-theme 'doom-vibrant t)

	;; Enable flashing mode-line on errors
	(doom-themes-visual-bell-config)

	;; Enable custom neotree theme (all-the-icons must be installed!)
	(doom-themes-neotree-config)

	;; Corrects (and improves) org-mode's native fontification.
	(doom-themes-org-config))

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-mode-swap-bg))

(provide 'init-doom)

;;; init-doom.el ends here
