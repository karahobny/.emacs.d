;;; -*- lexical-binding: t -*-
;;; my-helm-config.el --- helm keybindings mostly
;;; Commentary:
;;;            Funnily enough there already exists a `helm-config` file to be
;;;            required apparently, so I had to act coy in naming this and go
;;;            with the usual my-kebab-case-shebang.
;;;            Otherwise this really just concerns helm that I'm still trying
;;;            to learn how to utilize it to it's full potential..

;;; Code:
(use-package helm
  :defer  t
  :init   (helm-mode)
  :config (progn
            (require 'helm-config)
            (global-unset-key (kbd "M-x"))
            (setq helm-split-window-inside-p        t
                  helm-move-to-line-cycle-in-source t
                  helm-echo-input-in-header-line    t))
  :bind   ("M-x"     . helm-M-x)
          ("C-x C-m" . helm-M-x)
          ("C-c C-m" . helm-M-x)
          ("C-x C-f" . helm-find-files))

(provide 'my-helm-config)
;;; my-helm-config.el ends here
