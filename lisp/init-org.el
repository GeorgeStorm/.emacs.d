;;; init-org.el --- Orgmode setup

;;; Commentary:

;;; Code:

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind ("C-c a" . org-agenda)
  :config
  (setq org-agenda-tags-column -100
        org-tags-column -100
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-startup-truncated nil
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts t
        org-use-sub-superscripts '{}
        org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-caption-above nil
        org-export-with-smart-quotes t
        org-export-with-sub-superscripts nil))

(setq org-latex-minted-options '(("breaklines" "true")
                                 ("breakanywhere" "true")
                                 ("frame" "lines")))

(add-hook 'after-init-hook 'org-agenda-list)

(use-package org-ref
  :after org
  :init (setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/OneDrive/bibliography/notes.org"
      org-ref-default-bibliography '("~/OneDrive/bibliography/references.bib")
      org-ref-pdf-directory "~/OneDrive/bibliography/bibtex-pdfs/")

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(require 'org-ref-pdf)
(require 'org-ref-url-utils)

(provide 'init-org)

;;; init-org.el ends here
