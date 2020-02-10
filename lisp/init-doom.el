;;; init-doom.el --- Doom modeline and theme

;;; Commentary:

;;; Code:

(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-init)
  :custom
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-from-project)
  (doom-modeline-icon t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil))

(use-package doom-themes
	:config

	;; Global settings (defaults)
	(setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
		    doom-themes-enable-italic t ; if nil, italics is universally disabled
        )

	;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
	;; may have their own settings.
	(load-theme 'doom-dracula t)

	;; Enable flashing mode-line on errors
	(doom-themes-visual-bell-config)

	;; Enable custom neotree theme (all-the-icons must be installed!)
	;;(doom-themes-neotree-config)

  ;; Enable custom treemacs theme
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

	;; Corrects (and improves) org-mode's native fontification.
	(doom-themes-org-config)
  )

(provide 'init-doom)

;;; init-doom.el ends here
