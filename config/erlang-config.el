;;; erlang-config.el --- erlang/distel settings for EMACS
;;; Commentary:
;;;            Setting up erlang-mode, flycheck-hooks and distel.
;;;
;;;            Source:
;;;              http://www.lambdacat.com/post-modern-emacs-setup-for-erlang/

;;; Code:
;;; => distel
(push "~/.emacs.d/distel/elisp/" load-path)
(require 'distel)
(distel-setup)

(defvar inferior-erlang-prompt-timeout t)
(setq inferior-erlang-machine-options '("-sname" "emacs"))
(setq erl-nodename-cache
      (make-symbol
       (concat
        "emacs@"
        (car (split-string (shell-command-to-string "hostname"))))))

(provide 'erlang-config)
;;; erlang-config.el ends here
