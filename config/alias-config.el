;;; -*- lexical-binding: t -*-
;;; alias-config.el --- all things aliases
;;; Commentary:
;;;             Decided to move my `defaliases'-macro here instead of polluting
;;;             the init.el with unneccesary filler. Heavily inspired by some
;;;             examples set forth by Xah Lee

;;; Code:

(defmacro defaliases (&rest aliases)
  "Define ALIASES by looping through a list."
  `(progn
     ,@(mapcar (lambda (m)
                  (cl-multiple-value-bind (abbrev action)
                      (if (listp m)
                          (values (car m) (cdr m))
                        (values m))
                    `(defalias
                       (quote ,abbrev) (quote ,action))))
               aliases)))

;; => elisp
(defaliases
  (eb . eval-buffer)
  (er . eval-region)
  (ee . eval-expression)
  (elm . emacs-lisp-mode)
  (lim . lisp-interaction-mode)
  (eis . elisp-index-search)
  ;; => clojure
  (cctc . cider-connect-to-localhost)
  (ecc  . establish-cider-connection)
  ;; => &rest
  (package-list-packages . paradox-list-packages)
  (list-buffers          . ibuffer)
  (cc                    . calc))

(provide 'alias-config)
;;; alias-config.el ends here
