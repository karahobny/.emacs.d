;;; -*- lexical-binding: t; -*-
;;; fly-config.el --- flycheck and flymake specific configuration
;;; Commentary:
;;;            flycheck/flymake-hooks and defined checkers

;;; Code:
;; => flycheck
(use-package flycheck
  :defer  t
  :init   (progn
            (global-flycheck-mode)
            (flycheck-clojure-setup))
  :config (progn
            (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
            (add-hook 'flycheck-mode-hook #'flycheck-checkbashisms-setup)
            (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

;; => flymake
(use-package flymake-shell
  :defer    t
  :commands flymake-shell-load
  :init     (add-hook 'sh-set-shell-hook #'flymake-shell-load))

(provide 'fly-config)
;;; fly-config.el ends here
