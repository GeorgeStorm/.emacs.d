;;; init.el --- George's first Emacs config

;;; Commentary:
;; File containing my first attempt at an Emacs config from scratch

;;; Code:

;; Starting emacs server if one not already running
(setq server-auth-dir
      (let ((dir (concat user-emacs-directory
                         "server_" (format "%s_%s"
                                           emacs-major-version
                                           emacs-minor-version)
                         "_" (system-name)
                         "/")))
        (make-directory dir :parents)
        dir))

(require 'server)
(or (eq (server-running-p) t)
    (server-start))
(when (equal window-system 'w32)
  (setq server-use-tcp t))

(with-eval-after-load 'server
  (when (equal window-system 'w32)
    (defun server-ensure-safe-dir (dir) "Noop" t)))

;; Speed up startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; use-package setup
(require 'cl)
(require 'package)
(setq package-archives
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

(setq delete-old-versions -1  ; delete excess backups silently
      ;;version-control t
      ;;vc-make-backup-files t
      ;;vc-follow-symlinks t
      doc-view-continuous t ; At page edge goto next/previous.
      echo-keystrokes 0.1
      backup-directory-alist `(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
      inhibit-startup-screen t
      ring-bell-function 'ignore  ; silent bell on mistakes
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      sentence-end-double-space nil
      frame-title-format '("%m " invocation-name "@" system-name)
      initial-scratch-message nil )

(setq-default fill-column 100                        ; Maximum line width.
              indent-tabs-mode nil                   ; Use spaces instead of tabs.
              tab-width 2
              auto-fill-function nil)                ; Auto fill is annoying

(dolist (mode
         '(column-number-mode         ; Show column number in mode line.
           delete-selection-mode      ; Replace selected text.
           dirtrack-mode              ; directory tracking in *shell*
           show-paren-mode))          ; Highlight matching parentheses.
  (funcall mode 1))

(when (version< emacs-version "24.4")
  (eval-after-load 'auto-compile
    '((auto-compile-on-save-mode 1))))  ; compile .el files on save.

;; Better defaults
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-display-line-numbers-mode t )
(desktop-save-mode 1)
(display-time-mode 1)
(global-auto-revert-mode t)

;; show recent files in right click menu.
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

(use-package smart-mode-line)
(use-package all-the-icons
  :ensure t
  :defer t)
;; Allows export of orgmode files to html
(use-package htmlize
  :defer t)

;; show warnings in a small window
(setq display-buffer-alist
  '(("[*]Warnings[*]" .
     (display-buffer-in-side-window . '((side . bottom))))))

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

(run-with-idle-timer 300 t 'save-persistent-scratch)

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
 '(add-hook (quote prog-mode-hook) t)
 '(dashboard-startup-banner (quote logo) t)
 '(doom-modeline-buffer-file-name-style (quote buffer-name) t)
 '(doom-modeline-icon t t)
 '(doom-modeline-major-mode-icon t t)
 '(face-font-family-alternatives (quote (("Consolas" "Monaco" "Monospace"))))
 '(flyspell-delay 1 t)
 '(ispell-dictionary "en_GB" t)
 '(ispell-encoding8-command t t)
 '(ispell-program-name "hunspell" t)
 '(ispell-really-aspell nil t)
 '(ispell-really-hunspell t t)
 '(ispell-silently-savep t t)
 '(package-selected-packages
   (quote
    (omnisharp solaire-mode solarized-theme htmlize doom-modeline neotree smartparens which-key company flycheck counsel ivy all-the-icons use-package)))
 '(pdf-view-display-size (quote fit-page))
 '(pdf-view-resize-factor 1.1)
 '(pdf-view-use-unicode-ligther nil)
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face) t)
 '(rainbow-identifiers-cie-l*a*b*-lightness 70 t)
 '(rainbow-identifiers-cie-l*a*b*-saturation 20 t)
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
;; Syntax checking
(require 'init-flycheck)
;; Spell checking
(require 'init-flyspell)
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
;; C# in emacs?
(require 'init-omnisharp)
;; Org mode
(require 'init-org)
;; Project management
(require 'init-projectile)
;; Highlights parens etc
(require 'init-highlighting)
;; Changes ctrl-z to undo, uses undo-tree, adds ctrl-mousewheel to zoom in/out
(require 'init-remap)
;; Helps with finding inbuilt functions/key combos (chords?)
(require 'init-which-key)

;; Theme
(require 'init-doom)

;;; init.el ends here
