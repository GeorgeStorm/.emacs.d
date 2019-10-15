;;; init-random.el --- Handy functions etc.

;;; Commentary:

;;; Code:


;; Save scratch file (notepad++ empty file replacement)


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
(defun insert-date ()
   (interactive)
   (insert (format-time-string "%d.%m.%Y, %A")))

(global-set-key (kbd "C-c d") 'insert-date)

;; Use stackoverflow etc within emacs
(use-package howdoyou)

(provide 'init-random)

;;; init-random.el ends here
