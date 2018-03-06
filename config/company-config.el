;;; company-config.el --- company-mode related configuration
;;; Commentary:
;;;            company-mode doesn't need much configuration to begin with.

;;; Code:
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; => company-distel (for Erlang)
(require 'company-distel)
(add-hook 'erlang-mode-hook
          (lambda ()
            (setq company-backends '(company-distel))))

;; TODO: => keybindings?

(provide 'company-config)
;;; company-config.el ends here
