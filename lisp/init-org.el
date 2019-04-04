(setq org-todo-keyword-faces
      '(("TODO" . "brown")
        ("WORKING" . "DarkOrange")
        ("DONE" . "YellowGreen")))
(setq org-todo-keywords
  '((sequence "TODO(t)" "WORKING(w)"
              "DONE(d@/!)")))
(add-hook 'org-mode-hook #'toggle-word-wrap)
(setq org-startup-truncated nil)
(provide 'init-org)
