;;; init-git-diff.el --- Git Gutter setup

;;; Commentary:
;; File containing my setup for showing git status in fringe

;;; Code:

(use-package git-gutter
  :defer t
  :hook (prog-mode . git-gutter-mode)
  :init
  :config
  (setq git-gutter:disabled-modes '(org-mode asm-mode image-mode)
        git-gutter:update-interval 1
        git-gutter:window-width 2
        git-gutter:ask-p nil))


(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :after git-gutter
  :demand fringe-helper
  :config
  ;; subtle diff indicators in the fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
  nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
  nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
  [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
  nil nil 'center))

(provide 'init-git-diff)

;;; init-git-diff.el ends here
