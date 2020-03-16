;;; init-remap.el --- Key and behaviour remapping

;;; Commentary:
;;; Change C-scroll to zoom in and out like Windows
;;; Cycle through buffers
;;; Add undo-tree

;;; Code:

;; scrolling behavior
(setq mouse-wheel-scroll-amount '(10 ((shift) . 1))) ;; 10/1 line at a time
(setq mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; Scroll window under mouse

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)

;; page up/down
(global-set-key (kbd "<C-prior>") 'beginning-of-buffer)
(global-set-key (kbd "<C-next>") 'end-of-buffer)

;; move between frames.
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; Cycle through buffers with Ctrl-TAB/Ctrl-Shift-TAB
(global-set-key [C-tab] 'other-window)

;; Try new search function
(global-set-key (kbd "C-s") 'swiper-isearch)

;; Quicker find + replace binding
(global-set-key (kbd "C-r") 'query-replace-regexp)

;; Select all
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Switch to buffer in new frame
(global-set-key (kbd "C-x b") 'ido-switch-buffer-other-frame)

;; Function keys
;; Open eshell
(global-set-key [f6] 'shell-pop)
;; Open new emacs frame
(global-set-key [f7] 'make-frame)
;; File browser toggle
(global-set-key [f8] 'treemacs)

;; Better paste behaviour?
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)

;; Allow tree-semantics for undo operations.
(use-package undo-tree
  :diminish 'undo-tree-mode
  :config
  ;; Each node in the undo tree should have a timestamp.
  (setq undo-tree-visualizer-timestamps t
        ;; Show a diff window displaying changes between undo nodes.
        undo-tree-visualizer-diff t)
  (global-undo-tree-mode t))

;; make ctrl-z undo
(global-set-key (kbd "C-z") 'undo-tree-undo)
;; make ctrl-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)

;; Make C-g quit undo tree
(define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "<escape> <escape> <escape>") 'undo-tree-visualizer-quit)

(provide 'init-remap)

;;; init-remap.el ends here
