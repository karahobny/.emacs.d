;;; lisp-config.el --- all things lisp
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
(require 'slime)
(require 'slime-autoloads)
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl"))
        (clisp ("/usr/local/bin/clisp"))))
(setq slime-contribs '(slime-fancy))

;; ==> slime autocomplete
(require 'slime-company)
(slime-setup '(slime-company))

(provide 'lisp-config)
;;; lisp-config.el ends here