;; scrolling behavior
(setq mouse-wheel-scroll-amount '(5 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
;; scroll without moving point
(require 'smooth-scroll)
(global-set-key [(control down)] 'scroll-up-1)
(global-set-key [(control up)] 'scroll-down-1)
(global-set-key [(control left)] 'scroll-right-1)
(global-set-key [(control right)] 'scroll-left-1)

;; Make control-z undo
(let ((map (make-sparse-keymap)))
  ;; remap `undo' and `undo-only' to `undo-tree-undo'
  (define-key map [remap undo] 'undo-tree-undo)
  (define-key map [remap undo-only] 'undo-tree-undo)
  ;; bind standard undo bindings (since these match redo counterparts)
  (define-key map (kbd "C-z") 'undo-tree-undo)
  ;; (define-key map "\C-_" 'undo-tree-undo)
  ;; redo doesn't exist normally, so define our own keybindings
  (define-key map (kbd "C-?") 'undo-tree-redo)
  (define-key map (kbd "M-_") 'undo-tree-redo)
  (define-key map (kbd "C-S-z") 'undo-tree-redo)
  ;; just in case something has defined `redo'...
  (define-key map [remap redo] 'undo-tree-redo)
  ;; we use "C-x U" for the undo-tree visualizer
  (define-key map (kbd "\C-x U") 'undo-tree-visualize)
  ;; bind register commands
  (define-key map (kbd "C-x r u") 'undo-tree-save-state-to-register)
  (define-key map (kbd "C-x r U") 'undo-tree-restore-state-from-register)
  ;; set keymap
  (setq undo-tree-map map))

(global-undo-tree-mode t)

;; Make C-g quit undo tree
(define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "<escape> <escape> <escape>") 'undo-tree-visualizer-quit)

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
;; page up/down
(global-set-key (kbd "<C-prior>") 'beginning-of-buffer)
(global-set-key (kbd "<C-next>") 'end-of-buffer)

(provide 'init-remap)
