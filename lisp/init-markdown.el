;;; init-markdown.el --- Markdown setup

;;; Commentary:

;;; Code:

;; Used for markdown files
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "C:/Users/gwaldie/AppData/Local/Pandoc/pandoc.exe"))

(provide 'init-markdown)

;;; init-markdown.el ends here
