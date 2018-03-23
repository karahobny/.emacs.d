;;; -*- lexical-binding: t; -*-
;;; init-prog.el --- Miscellaneous programming tools and their settings

;;; Commentary:
;;;            Some of these might as well have been put in either init-lispy.el
;;;            or init-ml.el, but I'm not too nitpicky about it and neither should
;;;            you.

;;; Code:
;;;; COMPANY
(use-package company
  :defer t
  :init
  (with-no-warnings
    (global-company-mode))
  :bind
  (:map company-active-map
        ("tab" . company-complete))
  :config
  (setq company-selection-wrap-around t))

(use-package slime-company
  :defer t
  :init (slime-setup '(slime-company)))

(use-package company-distel
  :defer t
  :config
  (setq company-backends '(company-distel))
  :init
  (with-no-warnings
    (add-hook 'erlang-mode-hook #'company-distel)))

;;;; FLYCHECK
(use-package flycheck
  :defer t
  :init
  (with-no-warnings
    (add-hook 'flycheck-mode-hook #'flycheck-checkbashisms-setup)
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
    (global-flycheck-mode)
    (flycheck-clojure-setup))
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;;; GIT
(use-package magit
  :defer t
  :init
  (with-no-warnings
    (global-magit-file-mode))
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)
   ("C-c s" . magit-stage-file)
   ("C-c C-s" . magit-unstage-file)
   ("C-c c" . magit-commit-popup)
   ("C-c p" . magit-push-popup)))

;;;; PARENTHESES
(use-package parinfer
  :defer t
  :bind
  (:map parinfer-mode-map
        ("C-." . parinfer-toggle-mode))
  :config
  (setq parinfer-extensions
        '(defaults pretty-parens smart-yank paredit smart-tab))
  :init
  (with-no-warnings
    (dolist (hook '(clojure-mode-hook
                    emacs-lisp-mode-hook
                    common-lisp-mode-hook
                    scheme-mode-hook
                    lisp-mode-hook))
      (add-hook hook #'parinfer-mode))))

;;;; YASNIPPETS
(use-package yasnippet
  :defer t
  :init
  (with-no-warnings
    (add-hook 'clojure-mode-hook #'yas-minor-mode)))

(provide 'init-prog)
;;; init-prog.el ends here
