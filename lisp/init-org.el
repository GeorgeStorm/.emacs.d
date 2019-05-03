;;; init-org.el --- Orgmode setup

;;; Commentary:

;;; Code:

(setq org-src-tab-acts-natively t
      org-src-fontify-natively t)
(add-hook 'org-mode-hook #'toggle-word-wrap)
(setq org-startup-truncated nil)

(use-package org-ref
  :after org
  :init (setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f")))


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
