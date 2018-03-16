;;; ml-config.el --- SML and Ocaml configuration
;;; Commentary:
;;;            SML and Ocaml specific settings.

;;; Code:
;; => Standard ML
(use-package sml-mode
  :defer  t
  :mode   ("\\.\\(sml\\|sig\\)\\'" . sml-mode)
  :config (setq sml-program-name "mosml"
                sml-default-arg  "-P full"))

;; => Ocaml
(use-package tuareg
  :defer  t
  :config (setq tuareg-indent-align-with-first-arg t
                tuareg-match-patterns-aligned      t))

(provide 'ml-config)
;;; ml-config.el ends here
