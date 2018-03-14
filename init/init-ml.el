;;; -*- lexical-binding: t; -*-
;;; init-ml.el --- ML-family programming languages

;;; Commentary:
;;;            Yeah I'm stuffing Erlang here, so what.
;;;            It's just for the meantime.

;;; Code:
;;;; STANDARD ML
(use-package sml-mode
  :defer t
  :mode
  ("\\.\\(sml\\|sig\\)\\'" . sml-mode)
  :config
  (setq sml-program-name "mosml"
        sml-default-arg "-P full"))

;;;; OCAML
(use-package tuareg
  :defer t
  :config
  (setq tuareg-indent-align-with-first-arg t
        tuareg-match-patterns-aligned t))

;;;; ERLANG
(use-package erlang
  :defer t
  :config
  (defvar inferior-erlang-prompt-timeout t)
  (setq inferior-erlang-machine-options '("-sname" "emacs")))

(use-package distel
  :defer t
  :load-path "~/.emacs.d/distel/elisp"
  :config
  (with-no-warnings
    (distel-setup)))

(provide 'init-ml)
;;; init-ml.el ends here
