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
        ivy-count-format "%d/%d "
        ;; no regexp by default
        ivy-initial-inputs-alist nil
        ;; configure regexp engine.
        ivy-re-builders-alist
	      ;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

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

(use-package all-the-icons-ivy
  :ensure t
  :config
  (setq all-the-icons-ivy-buffer-commands
        '(counsel-switch-buffer ido-switch-buffer-other-frame))
  (all-the-icons-ivy-setup))

(use-package ivy-rich
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
   	    (get-buffer candidate)
	    (let ((icon (all-the-icons-icon-for-mode major-mode)))
	      (if (symbolp icon)
	          (all-the-icons-icon-for-mode 'fundamental-mode)
	        icon))))
  :init
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30))  ; return the candidate itself
            (ivy-rich-switch-buffer-size (:width 7))  ; return the buffer size
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
            (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
           :predicate
           (lambda (cand) (get-buffer cand)))
        counsel-M-x
        (:columns
         ((counsel-M-x-transformer (:width 40))  ; thr original transformer
          (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
        counsel-describe-function
        (:columns
         ((counsel-describe-function-transformer (:width 40))  ; the original transformer
          (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
        counsel-describe-variable
        (:columns
         ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
          (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
        counsel-recentf
        (:columns
         ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
          (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))) ; return the last modified time of the file
  )
:config
(ivy-rich-mode +1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(provide 'init-ivy)

;;; init-ivy.el ends here
