;;; c-config.el --- setting c mode to your liking
;;; Commentary:
;;;            C-mode set to use tabs and style as `BSD`-variant

;;; Code:
(defun my-c-mode-hook ()
  "Hook for C-mode."
  ;; => tabs instead of spaces
  (setq c-tab-always-indent t
        indent-tabs-mode t
        c-basic-offset 4
        tab-width 4)

  ;; => text manipulation
  (define-key c-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key c-mode-map "\C-ce" 'c-comment-edit)
  (setq c-auto-hungry-initial-state 'none
        c-delete-function 'backward-delete-char)

  ;; => coding style
  (setq c-default-style "bsd"))

;; => turning tricks
(add-hook 'c-mode-hook 'my-c-mode-hook)

(provide 'c-config)
;;; c-config.el ends here
