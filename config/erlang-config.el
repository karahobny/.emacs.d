;;; -*- lexical-binding: t -*-
;;; erlang-config.el --- erlang/distel settings for EMACS
;;; Commentary:
;;;            Setting up erlang-mode, flycheck-hooks and distel.
;;;
;;;            Source:
;;;              http://www.lambdacat.com/post-modern-emacs-setup-for-erlang/

;;; Code:
;;; => distel
(use-package distel
  :defer     t
  :load-path "~/.emacs.d/distel/elisp"
  :config      (distel-setup))

(use-package erlang
  :config (defvar inferior-erlang-prompt-timeout   t)
          (setq   inferior-erlang-machine-options '("-sname" "emacs")))

(provide 'erlang-config)
;;; erlang-config.el ends here
