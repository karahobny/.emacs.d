;;; c-config.el --- setting c mode to your liking
;;; Commentary:
;;;            C-mode set to use tabs and style as `BSD`-variant.
;;;            Not tested in it's current form. Might be broken as shit.

;;; Code:
(use-package cc-mode
  :defer t
  :config (progn
            (setq c-default-style     "bsd"
                  c-tab-always-indent t
                  indent-tabs-mode    t
                  c-basic-offset      4
                  tab-width           4
                  c-delete-function   'backward-delete-char))
  :bind (:map c-mode-map
              ("\C-m"  . reindent-then-newline-and-indent)
              ("\C-ce" . c-comment-edit)))

(provide 'c-config)
;;; c-config.el ends here
