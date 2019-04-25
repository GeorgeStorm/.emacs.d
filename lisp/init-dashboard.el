;;; init-dashboard.el --- Displays an intro screen on opening

;;; Commentary:

;;; Code:

(use-package dashboard
  :if (< (length command-line-args) 2)
  :preface
  (defun my/dashboard-banner ()
    "Sets a dashboard banner including information on package initialization
     time and garbage collections."
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %.2f seconds with %d garbage collections."
                  (float-time
                   (time-subtract after-init-time before-init-time)) gcs-done)))
  (setq dashboard-items '((recents  . 15)))
  :init
    (add-hook 'after-init-hook 'dashboard-refresh-buffer)
    (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
  :custom (dashboard-startup-banner 'logo)
  :config (dashboard-setup-startup-hook))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
