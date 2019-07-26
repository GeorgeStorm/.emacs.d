;;; init-ivy.el --- Command completion

;;; Commentary:

;;; Code:
(use-package ivy
  :diminish (ivy-mode . "")
  :init (ivy-mode 1) ;; Globally at startup
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-height 20
        ivy-count-format "%d/%d "))

;; Override the basic Emacs commands
(use-package counsel
  :bind* ;; Simplified some common function keymaps
  (("M-x"     . counsel-M-x)
   ("C-s"     . swiper) ;; Search file
   ("C-f"     . counsel-find-file) ;; Find file
   ("C-b"     . counsel-switch-buffer) ;; Switch buffer
   ("C-x C-r" . counsel-recentf) ;; Search for recently edited
   ("C-c g"   . counsel-git) ;; Search for files in git repo
   ("C-c s"   . counsel-git-grep) ;; Search for regexp in git repo
   ("<f1> f"  . counsel-describe-function)
   ("<f1> v"  . counsel-describe-variable)
   ("<f1> l"  . counsel-find-library)
   ("<f2> i"  . counsel-info-lookup-symbol)
   ("<f2> u"  . counsel-unicode-char)
   ("C-c C-r" . ivy-resume))) ;; Resume last ivy-based completion
   
(provide 'init-ivy)

;;; init-ivy.el ends here
