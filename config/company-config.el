;;; -*- lexical-binding: t; -*-
;;; company-config.el --- company-mode related configuration
;;; Commentary:
;;;            company-mode doesn't need much configuration to begin with.

;;; Code:
(use-package company
  :defer  t
  :init   (global-company-mode)
  :bind   (:map company-active-map
            ("tab" . company-complete))
  :config (setq company-selection-wrap-around t))

;; => slime
(use-package slime-company
  :defer t
  :init  (slime-setup '(slime-company)))

;; => erlang
(use-package company-distel
  :defer t
  :init  (setq company-backends '(company-distel))
  :hook  (erlang-mode-hook))

(provide 'company-config)
;;; company-config.el ends here
