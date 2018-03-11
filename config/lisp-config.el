;;; lisp-config.el --- lisp and elisp
;;; Commentary:
;;;            SLIMEY BLIMEY GRIMEY

;;; Code:
;; => aliases
(defalias 'eb  #'eval-buffer)
(defalias 'er  #'eval-region)
(defalias 'ee  #'eval-expression)
(defalias 'elm #'emacs-lisp-mode)
(defalias 'lim #'lisp-interaction-mode)
(defalias 'eis #'elisp-index-search)

;; => slime
(use-package slime
  :defer t
  :config
  (setq slime-lisp-implementations
        '((sbcl ("/usr/local/bin/sbcl"))
          (clisp ("/usr/local/bin/clisp"))))
  (setq slime-contribs '(slime-fancy)))

(provide 'lisp-config)
;;; lisp-config.el ends here
