;;; parinfer-config.el --- extensions, hooks, etc.
;;; Commentary:
;;;            parinfer needs to be hooked to programming
;;;            languages of your choosing.  A keybinding to
;;;            handle toggling between Paren and Indent
;;;            -modes wouldn't harm you either.

;;; Code:
(setq parinfer-extensions '(defaults pretty-parens smart-yank paredit)
      parinfer-auto-switch-indent-mode 0)

;; => keybindings
(global-set-key (kbd "\C-ct") #'parinfer-toggle-mode)

;; => parinfer hooks
(add-hook 'clojure-mode-hook     #'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook  #'parinfer-mode)
(add-hook 'common-lisp-mode-hook #'parinfer-mode)
(add-hook 'lisp-mode-hook        #'parinfer-mode)
(add-hook 'scheme-mode-hook      #'parinfer-mode)

(provide 'parinfer-config)
;;; parinfer-config.el ends here
