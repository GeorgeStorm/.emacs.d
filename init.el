;;; init.el --- George's first Emacs config

;;; Commentary:
;; File containing my first attempt at an Emacs config from scratch

;;; Code:

;; Starting emacs server if one not already running
;; Set EMACS_SERVER_FILE to "C:\Users\Georg\AppData\Roaming\.emacs.d\server"
(require 'server)
(unless (server-running-p)
  (cond
   ((eq system-type 'windows-nt)
    (setq server-auth-dir "~\\.config\\emacs\\server\\"))
   ((eq system-type 'gnu/linux)
    (setq server-auth-dir "~/.config/emacs/server/")))
  (setq server-name "emacs-server-file")
  (server-start))

;; Speed up startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(setq file-name-handler-alist-original file-name-handler-alist
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          `(lambda ()
             (setq gc-cons-threshold 16777216
                   gc-cons-percentage 0.1)
             (setq file-name-handler-alist file-name-handler-alist-original)
             (makunbound 'file-name-handler-alist-original)
             (garbage-collect)) t)

;; use-package setup
(require 'cl)
(require 'package)
(setq-default package-archives
              `(,@package-archives
                ("melpa" . "https://melpa.org/packages/")
                ("melpa_alt" . "http://melpa.milkbox.net/packages/")
                ("org" . "http://orgmode.org/elpa/")
                ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(setq-default use-package-always-ensure t ;; Auto download if needed
              use-package-verbose nil ;; Don't report loading details
              use-package-expand-minimally t ;; Make the expanded code as minimal as possible
              )
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package bind-key
  :ensure t)

(setq delete-old-versions -1 ;; Delete excess backups silently
      version-control t
      vc-make-backup-files t
      vc-follow-symlinks t
      doc-view-continuous t ;; At page edge goto next/previous.
      echo-keystrokes 0.1
      backup-directory-alist `(("." . "~/.config/emacs/backups"))
      auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t))
      inhibit-startup-screen t
      ring-bell-function 'ignore ;; Silent bell on mistakes
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      sentence-end-double-space nil
      frame-title-format '("%m " invocation-name "@" system-name)
      initial-scratch-message nil)

(setq-default fill-column 80 ;; Maximum line width.
              indent-tabs-mode nil ;; Use spaces instead of tabs.
              tab-width 2) ;; Size of tab in spaces

;; Set default font
(set-face-attribute 'default nil
                    :font "DejaVu Sans Mono"
                    :height 110
                    :weight 'normal
                    :width 'normal)

(set-face-attribute 'fixed-pitch nil
                    :font "DejaVu Sans Mono"
                    :height 110
                    :weight 'normal
                    :width 'normal)

(set-face-attribute 'variable-pitch nil
                    :font "DejaVu Serif"
                    :height 110
                    :weight 'light
                    :width 'normal)

(add-hook 'text-mode-hook 'turn-on-auto-fill) ;; Enable auto-fill for text buffers
(add-hook 'text-mode-hook 'variable-pitch-mode)
(add-hook 'magit-mode-hook 'variable-pitch-mode)

;; Better defaults
(tool-bar-mode -1) ;; Disable tool bar
(scroll-bar-mode -1) ;; Disable scroll bar
(menu-bar-mode -1) ;; Disable menu
(tooltip-mode -1) ;; Disable tooltips
(fset 'yes-or-no-p 'y-or-n-p) ;; Only required to type y/n instead of yes/no
(desktop-save-mode 1) ;; Save previous buffers/frames
(display-time-mode 0) ;; Do/do not display the time in the modeline
(global-auto-revert-mode t) ;; Automatically update buffers if changes on disk
(column-number-mode t) ;; Display the current column in the modeline
(delete-selection-mode t) ;; Deletes selected text when typing
(show-paren-mode t) ;; Highlights matching paren

;; show recent files in right click menu.
(use-package recentf
  :ensure t
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 25
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

;; Required for doom-themes
(use-package all-the-icons
  :ensure t
  :defer t)

(use-package powershell
    :ensure t)

;; Compilation flags
(setq-default
 compilation-auto-jump-to-first-error t    ; Take me to the first error
 compilation-always-kill t                 ; Restart compilation without prompt
 compilation-scroll-output 'first-error)   ; Follow compilation buffer until we hit an error

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t t)
 '(TeX-auto-save t t)
 '(TeX-byte-compile t t)
 '(TeX-clean-confirm nil t)
 '(TeX-master 'dwim t)
 '(TeX-parse-self t t)
 '(TeX-source-correlate-mode t t)
 '(TeX-view-program-selection '((output-pdf "PDF Tools")) t)
 '(add-hook 'prog-mode-hook t)
 '(custom-safe-themes
   '("ca849ae0c889eb918785cdc75452b1e11a00848a5128a95a23872e0119ccc8f4" "7b50dc95a32cadd584bda3f40577e135c392cd7fb286a468ba4236787d295f4b" "f2b83b9388b1a57f6286153130ee704243870d40ae9ec931d0a1798a5a916e76" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "9f08dacc5b23d5eaec9cccb6b3d342bd4fdb05faf144bdcd9c4b5859ac173538" "0f1733ad53138ddd381267b4033bcb07f5e75cd7f22089c7e650f1bb28fc67f4" "f07729f5245b3c8b3c9bd1780cbe6f3028a9e1ed45cad7a15dd1a7323492b717" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "a9d67f7c030b3fa6e58e4580438759942185951e9438dd45f2c668c8d7ab2caf" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "886fe9a7e4f5194f1c9b1438955a9776ff849f9e2f2bbb4fa7ed8879cdca0631" "ef07cb337554ffebfccff8052827c4a9d55dc2d0bc7f08804470451385d41c5c" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(dashboard-startup-banner 'logo t)
 '(doom-modeline-bar-width 6)
 '(doom-modeline-buffer-encoding nil)
 '(doom-modeline-buffer-file-name-style 'truncate-from-project nil nil "Customized with use-package doom-modeline")
 '(doom-modeline-buffer-modification-icon t nil nil "Customized with use-package doom-modeline")
 '(doom-modeline-buffer-state-icon t nil nil "Customized with use-package doom-modeline")
 '(doom-modeline-enable-word-count t nil nil "Customized with use-package doom-modeline")
 '(doom-modeline-env-version nil)
 '(doom-modeline-icon t nil nil "Customized with use-package doom-modeline")
 '(doom-modeline-indent-info nil)
 '(doom-modeline-major-mode-color-icon t nil nil "Customized with use-package doom-modeline")
 '(doom-modeline-major-mode-icon t nil nil "Customized with use-package doom-modeline")
 '(flyspell-delay 1)
 '(ispell-dictionary "en_GB")
 '(ispell-encoding8-command t t)
 '(ispell-program-name "hunspell")
 '(ispell-really-aspell nil t)
 '(ispell-really-hunspell t t)
 '(ispell-silently-savep t)
 '(ivy-prescient-retain-classic-highlighting t)
 '(org-agenda-files nil)
 '(org-agenda-tags-column -100)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-fontify-whole-heading-line nil)
 '(org-hide-leading-stars t)
 '(org-tags-column -100)
 '(package-selected-packages
   '(powershell markdown-mode ivy-rich company-prescient ivy-prescient prescient treemacs ox-pandoc org help-find-org-mode dracula-theme shell-pop color-theme-sanityinc-tomorrow omnisharp solaire-mode solarized-theme htmlize doom-modeline smartparens which-key company flycheck counsel ivy all-the-icons use-package))
 '(pdf-view-display-size 'fit-page)
 '(pdf-view-resize-factor 1.1)
 '(pdf-view-use-unicode-ligther nil)
 '(show-paren-delay 0)
 '(sp-escape-quotes-after-insert nil t)
 '(tooltip-mode -1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "MediumPurple4"))))
 '(minibuffer-prompt ((t (:foreground "#bd93f9"))))
 '(mode-line ((t (:background "#522740" :box nil))))
 '(org-block ((t (:background "#23242f" :foreground "#bd93f9" :family "DejaVu Sans Mono"))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((t (:foreground "#6272a4" :strike-through t))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-table ((t (:foreground "#bd93f9" :family "DejaVu Sans Mono"))))
 '(show-paren-match ((t (:background "yellow" :foreground "#ff5555" :weight normal)))))

 ;; Pull in ./lisp/*
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; Autocomplete
(require 'init-company)
;; Dashboard
;; (require 'init-dashboard)
;; C# in emacs?
(require 'init-dotnet)
;; Syntax checking
(require 'init-flycheck)
;; Spell checking
(require 'init-flyspell)
;; Shows git status in fringe
(require 'init-git-diff)
;; Highlights parens etc
(require 'init-highlighting)
;; Searching
(require 'init-ivy)
;; Latex/PDF mode
(require 'init-latex)
;; GIT in emacs
(require 'init-magit)
;; Markdown mode
(require 'init-markdown)
;; File explorer
(require 'init-treemacs)
;; Org mode
(require 'init-org)
;; Project management
(require 'init-projectile)
;; Python mode
(require 'init-python)
;; Contains various functions to keep main init.el tidy
(require 'init-random)
;; Changes ctrl-z to undo, uses undo-tree, adds ctrl-mousewheel to zoom in/out
(require 'init-remap)
;; Helps with finding inbuilt functions/key combos (chords?)
(require 'init-which-key)

;; Theme
(require 'init-doom)

;;; init.el ends here
