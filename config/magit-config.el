;;; -*- lexical-binding: t; -*-
;;; magit-config.el --- globalizing magit and binding it
;;; Commentary:
;;;            magit is sweet, but it needs bindings I can get into.

;;; Code:
(use-package magit
  :defer t
  :init  (global-magit-file-mode)
  :bind  ("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         ("C-c s"   . magit-stage-file)
         ("C-c C-s" . magit-unstage-file)
         ("C-c c"   . magit-commit-popup)
         ("C-c p"   . magit-push-popup))

(provide 'magit-config)
;;; magit-config.el ends here
