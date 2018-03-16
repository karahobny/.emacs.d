;;; -*- lexical-binding: t; -*-
;;; company-config.el --- company-mode related configuration
;;; Commentary:
;;;            company-mode doesn't need much configuration to begin with.

;;; Code:
(use-package company
  :defer  t
  :init   (with-no-warnings
            (progn
              (global-company-mode)))
  :bind   (:map company-active-map
                ("tab" . company-complete))
  :config (setq company-selection-wrap-around t))

;; => slime
(use-package slime-company
  :defer t
  :init  (slime-setup '(slime-company)))

;; => erlang
(use-package company-distel
  :defer  t
  :config (setq company-backends '(company-distel))
  :init   (with-no-warnings
            (progn
              (add-hook 'erlang-mode-hook #'company-distel))))

(provide 'company-config)
;;; company-config.el ends here
