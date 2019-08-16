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

;; scroll without moving point
(require 'smooth-scroll)
(global-set-key [(control down)] 'scroll-up-1)
(global-set-key [(control up)] 'scroll-down-1)
(global-set-key [(control left)] 'scroll-right-1)
(global-set-key [(control right)] 'scroll-left-1)

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)

;; page up/down
(global-set-key (kbd "<C-prior>") 'beginning-of-buffer)
(global-set-key (kbd "<C-next>") 'end-of-buffer)

;; Cycle through buffers with Ctrl-TAB/Ctrl-Shift-TAB
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-tab] 'previous-buffer)

;; Try new search function
(global-set-key (kbd "C-s") 'swiper-isearch)

;; Quicker find + replace binding
(global-set-key (kbd "C-r") 'query-replace-regexp)

;; Switch to buffer in new frame
(global-set-key (kbd "C-x b") 'ido-switch-buffer-other-frame)

;; Function keys
;; Open new emacs frame
(global-set-key [f7] 'make-frame)
;; File browser toggle
(global-set-key [f8] 'neotree-toggle)
;; Open Shell
(global-set-key [f9] 'shell-pop)

;; Better paste behaviour?
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)

;; Make control-z undo
(let ((map (make-sparse-keymap)))
  ;; remap `undo' and `undo-only' to `undo-tree-undo'
  (define-key map [remap undo] 'undo-tree-undo)
  (define-key map [remap undo-only] 'undo-tree-undo)
  ;; bind standard undo bindings (since these match redo counterparts)
  (define-key map (kbd "C-z") 'undo-tree-undo)
  (define-key map (kbd "C-S-z") 'undo-tree-redo)
  ;; just in case something has defined `redo'...
  (define-key map [remap redo] 'undo-tree-redo)
  ;; we use "C-x U" for the undo-tree visualizer
  (define-key map (kbd "C-x U") 'undo-tree-visualize)
  ;; bind register commands
  (define-key map (kbd "C-x r u") 'undo-tree-save-state-to-register)
  (define-key map (kbd "C-x r U") 'undo-tree-restore-state-from-register)
  ;; set keymap
  (setq undo-tree-map map))

(global-undo-tree-mode t)

;; Make C-g quit undo tree
(define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "<escape> <escape> <escape>") 'undo-tree-visualizer-quit)

(provide 'init-remap)

;;; init-remap.el ends here
