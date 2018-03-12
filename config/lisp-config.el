;;; lisp-config.el --- lisp and elisp
;;; Commentary:
;;;            SLIMEY BLIMEY GRIMEY

;;; Code:
;; => aliases
(defaliases
  (eb . eval-buffer)
  (er . eval-region)
  (ee . eval-expression)
  (elm . emacs-lisp-mode)
  (lim . lisp-interaction-mode)
  (eis . elisp-index-search))

;; => slime
(use-package slime
  :defer t
  :config
  (setq slime-lisp-implementations
        '((sbcl ("/usr/local/bin/sbcl"))
          (clisp ("/usr/local/bin/clisp"))))
  (setq slime-contribs '(slime-fancy)))

;; => initialize new emacs lisp files
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (when (and (bobp) (eobp))
;;               (setq lexical-binding t)
;;               (insert ";;; -*- lexical-binding: t; -*-\n")))

(provide 'lisp-config)
;;; lisp-config.el ends here
