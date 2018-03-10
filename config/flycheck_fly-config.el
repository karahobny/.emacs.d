;;; fly-config.el --- flycheck and flymake specific configuration
;;; Commentary:
;;;            flycheck/flymake-hooks and defined checkers

;;; Code:
;; => flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  '(flycheck-clojure-setup)
  '(add-hook 'flycheck-mode-hook #'flycheck-checkbashisms-setup)
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; => flymake
(require 'flymake-shell)
(add-hook 'sh-set-shell-hook 'flymake-shell-load)

(provide 'fly-config)
;;; fly-config.el ends here
