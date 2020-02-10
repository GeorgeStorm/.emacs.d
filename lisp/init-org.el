;;; init-org.el --- Orgmode setup

;;; Commentary:

;;; Code:

;; Allows export of orgmode files to html
(use-package htmlize
  :defer t)

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c C-l" . org-insert-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq org-todo-keywords
        '((sequence "TODO" "DOING" "|" "DONE" "POSTPONED"))
        org-todo-keyword-faces
        '(("TODO" .  "orange" )
          ("DOING" .  "yellow" )
          ("DONE" . "PaleGreen")
          ("POSTPONED" .  "red"))

        org-link-file-path-type 'relative
        org-startup-indented t
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-fontify-done-headline t
        org-startup-truncated nil
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts t
        org-use-sub-superscripts '{}
        org-export-with-sub-superscripts nil
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-pretty-entities t))

(custom-set-faces
 '(org-headline-done
            ((((class color) (min-colors 16) (background dark))
              (:strike-through t))))
 '(org-headline-done ((t (:foreground "#6272b2" :strike-through t)))))

;; Generic batch org to markdown converter
(defun publish-dir-org ()
  "Publish all org files in a directory"
  (interactive)
  (save-excursion
    (mapc
     (lambda (file)
       (with-current-buffer
       (find-file-noselect file)
     (org-md-export-to-markdown)))
       (file-expand-wildcards  "*.org"))))

(use-package org-sidebar)

;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (shell . t)
   (python . t)
   (sql . t)
   (js . t)))

(use-package org-ref
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :after org
  :init (setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f")))

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

;; Monospaced fonts for src blocks and tables in org-mode
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-block nil :inherit 'fixed-pitch)

(provide 'init-org)

;;; init-org.el ends here
