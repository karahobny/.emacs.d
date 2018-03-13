;;; -*- lexical-binding: t -*-
;;; parinfer-config.el --- extensions, hooks, etc.
;;; Commentary:
;;;            parinfer needs to be hooked to programming
;;;            languages of your choosing.  A keybinding to
;;;            handle toggling between Paren and Indent
;;;            -modes wouldn't harm you either.

;;; Code:
(use-package parinfer
  :defer    t
  :commands parinfer-mode
  :bind     (:map parinfer-mode-map
              ("C-." . parinfer-toggle-mode))
  :config   (progn
              (setq parinfer-extensions
                    '(defaults pretty-parens smart-yank paredit smart-tab)))
  :init     (progn
              (add-hook 'clojure-mode-hook     #'parinfer-mode)
              (add-hook 'emacs-lisp-mode-hook  #'parinfer-mode)
              (add-hook 'common-lisp-mode-hook #'parinfer-mode)
              (add-hook 'scheme-mode-hook      #'parinfer-mode)
              (add-hook 'lisp-mode-hook        #'parinfer-mode)))

(provide 'parinfer-config)
;;; parinfer-config.el ends here
