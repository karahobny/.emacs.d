;;; ml-config.el --- SML and Ocaml configuration
;;; Commentary:
;;;            SML and Ocaml specific settings.

;;; Code:
;; => Standard ML
(require 'sml-mode)
(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))
(setq sml-program-name "mosml"
      sml-default-arg  "-P full")

;; => Ocaml
(require 'tuareg)
(setq tuareg-indent-align-with-first-arg t
      tuareg-match-patterns-aligned t)

(provide 'ml-config)
;;; ml-config.el ends here
