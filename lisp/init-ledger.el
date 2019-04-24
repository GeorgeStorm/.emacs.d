;;; init-ledger.el --- Money management

;;; Commentary:

;;; Code:

(use-package ledger-mode
  :mode "\\.dat\\'"
  :init
  (setq aw/ledger-dir "~/Dropbox/ledger/data/")
  (setq ledger-clear-whole-transactions 1)

  :config
  (defun aw/clean-ledger-on-save ()
    (interactive)
    (if (eq major-mode 'ledger-mode)
        (let ((curr-line (line-number-at-pos)))
          (ledger-mode-clean-buffer)
          (line-move (- curr-line 1)))))

  (defun ledger-increment-date ()
    (interactive)
    (aw/ledger-change-date 1))

  (defun ledger-decrement-date ()
    (interactive)
    (aw/ledger-change-date -1))

  (defun aw/ledger-change-date (num)
    "Replace date of current transaction with date + num days.
   Currently only works with the format %Y/%m/%d"
    (save-excursion
      (ledger-navigate-beginning-of-xact)
      (let* ((beg (point))
             (end (re-search-forward ledger-iso-date-regexp))
             (xact-date (filter-buffer-substring beg end)))
        (delete-region beg end)
        (insert
         (format-time-string
          "%Y/%m/%d"
          (time-add (aw/encoded-date xact-date)
                    (days-to-time num)))))))

  (defun aw/encoded-date (date)
    "Given a date in the form %Y/%m/%d, return encoded time string"
    (string-match "\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" date)
    (let* ((fixed-date
            (concat (match-string 1 date) "-" (match-string 2 date) "-" (match-string 3 date)))
           (d (parse-time-string fixed-date)))
      (encode-time 0 0 0 (nth 3 d) (nth 4 d) (nth 5 d)))))

(use-package flycheck-ledger
  :config
  (add-hook 'ledger-mode-hook 'flycheck-mode))

(provide 'init-ledger)

;;; init-ledger.el ends here
