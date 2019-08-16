;;; init.el --- George's first Emacs config

;;; Commentary:
;; File containing my first attempt at an Emacs config from scratch

;;; Code:

;; Starting emacs server if one not already running
(require 'server)
(unless (server-running-p)
  (cond
   ((eq system-type 'windows-nt)
    (setq server-auth-dir "~\\.emacs.d\\server\\"))
   ((eq system-type 'gnu/linux)
    (setq server-auth-dir "~/.emacs.d/server/")))
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
        ("org" . "http://orgmode.org/elpa/")
         ("gnu" . "https://elpa.gnu.org/packages/")
     ("MELPA" . "https://melpa.org/packages/")))
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

;; Allows for the tweaking of startup
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package bind-key
  :ensure t)

(setq delete-old-versions -1 ;; Delete excess backups silently
      version-control t
      vc-make-backup-files t
      vc-follow-symlinks t
      doc-view-continuous t ;; At page edge goto next/previous.
      echo-keystrokes 0.1
      backup-directory-alist `(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
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

(add-hook 'text-mode-hook 'turn-on-auto-fill) ;; Enable auto-fill for text buffers


;; Set default font
(set-face-attribute 'default nil :family "Consolas" :height 100 )

;; Better defaults
(tool-bar-mode -1) ;; Disable tool bar
(scroll-bar-mode -1) ;; Disable scroll bar
(menu-bar-mode -1) ;; Disable menu
(tooltip-mode -1) ;; Disable tooltips
(fset 'yes-or-no-p 'y-or-n-p) ;; Only required to type y/n instead of yes/no
(global-display-line-numbers-mode t ) ;; Display line numbers
(desktop-save-mode 1) ;; Save previous buffers/frames
(display-time-mode 1) ;; Display the time in the modeline
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

(use-package smart-mode-line) ;; Nicer looking modeline

;; Required for doom-themes
(use-package all-the-icons
  :ensure t
  :defer t)

;; Used to track how often commands are used, to help improve effi by changing keymaps
(use-package keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(use-package shell-pop  ;; Allows for small shell window to be made easily
  :config
  (setq shell-pop-full-span t ;; Shell windows spans entire emacs frame
        shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell
                                                                      shell-pop-term-shell))))))
  ;; Using eshell as the default (platform agnostic)
(shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)

;; Show warnings in a small window on opening
(setq display-buffer-alist
  '(("[*]Warnings[*]" .
     (display-buffer-in-side-window . '((side . bottom))))))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (yas-global-mode))

;; Save scratch file (notepad++ empty file replacement)
(defun save-persistent-scratch ()
       (with-current-buffer (get-buffer-create "*scratch*")
         (write-region (point-min) (point-max)
                       (concat user-emacs-directory "scratch"))))

(defun load-persistent-scratch ()
  (let ((scratch-file (concat user-emacs-directory "scratch")))
    (if (file-exists-p scratch-file)
        (with-current-buffer (get-buffer "*scratch*")
          (delete-region (point-min) (point-max))
          (insert-file-contents scratch-file)))))

(add-hook 'emacs-startup-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)
(run-with-idle-timer 180 t 'save-persistent-scratch) ;; Save the scratch buffer every 5mins

;; Insert date (for org notes files)
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%d.%m.%Y")
                   ((equal prefix '(4)) "%Y-%m-%d")))
          (system-time-locale "de_DE"))
      (insert (format-time-string format))))

(global-set-key (kbd "C-c d") 'insert-date)

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
 '(TeX-master (quote dwim) t)
 '(TeX-parse-self t t)
 '(TeX-source-correlate-mode t t)
 '(TeX-view-program-selection (quote ((output-pdf "PDF Tools"))) t)
 '(add-hook (quote flyspell-mode-hook) t)
 '(custom-safe-themes
   (quote
    ("274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "9f08dacc5b23d5eaec9cccb6b3d342bd4fdb05faf144bdcd9c4b5859ac173538" "0f1733ad53138ddd381267b4033bcb07f5e75cd7f22089c7e650f1bb28fc67f4" "f07729f5245b3c8b3c9bd1780cbe6f3028a9e1ed45cad7a15dd1a7323492b717" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "a9d67f7c030b3fa6e58e4580438759942185951e9438dd45f2c668c8d7ab2caf" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "886fe9a7e4f5194f1c9b1438955a9776ff849f9e2f2bbb4fa7ed8879cdca0631" "ef07cb337554ffebfccff8052827c4a9d55dc2d0bc7f08804470451385d41c5c" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(dashboard-startup-banner (quote logo) t)
 '(doom-modeline-buffer-file-name-style (quote buffer-name) t)
 '(doom-modeline-enable-word-count t t)
 '(doom-modeline-icon t t)
 '(doom-modeline-major-mode-icon t t)
 '(flyspell-delay 1 t)
 '(ispell-dictionary "en_GB" t)
 '(ispell-encoding8-command t t)
 '(ispell-program-name "hunspell" t)
 '(ispell-really-aspell nil t)
 '(ispell-really-hunspell t t)
 '(ispell-silently-savep t t)
 '(org-agenda-files nil)
 '(org-agenda-tags-column -100 t)
 '(org-tags-column -100)
 '(package-selected-packages
   (quote
    (dracula-theme shell-pop color-theme-sanityinc-tomorrow omnisharp solaire-mode solarized-theme htmlize doom-modeline neotree smartparens which-key company flycheck counsel ivy all-the-icons use-package)))
 '(pdf-view-display-size (quote fit-page))
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
 )

 ;; Pull in ./lisp/*
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; Autocomplete
(require 'init-company)
;; Startup dashboard
(require 'init-dashboard)
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
;; Ledger mode
(require 'init-ledger)
;; GIT in emacs
(require 'init-magit)
;; File explorer
(require 'init-neotree)
;; Org mode
(require 'init-org)
;; Project management
(require 'init-projectile)
;; Python mode
(require 'init-python)
;; Changes ctrl-z to undo, uses undo-tree, adds ctrl-mousewheel to zoom in/out
(require 'init-remap)
;; Helps with finding inbuilt functions/key combos (chords?)
(require 'init-which-key)

;; Theme
(require 'init-doom)

;;; init.el ends here
