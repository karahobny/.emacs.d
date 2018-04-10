;;; -*- lexical-binding: t; -*-
;;; init-prog.el --- Code autocompletion, language-specific modes, syntax checking.

;;; Commentary:
;;;            Programming language-specific modes and their configuration.
;;;            Code (and pretty much everything else) is autocompleted with
;;;            company.el and syntax checking through flycheck.el. Version
;;             controlling through magit.el of course.

;;; Code:
;;;; *** COMPANY ***
(use-package company
  :defer  t
  :bind   (:map company-active-map
                ("tab" . company-complete))
  :config (progn
            (setq company-minimum-prefix-length 2
                  company-selection-wrap-around t
                  company-show-numbers          t
                  company-idle-delay            0.2)
            (global-company-mode)))

(use-package slime-company
  :defer t
  :init  (progn
           (slime-setup '(slime-company))))

;;;; *** FLYCHECK ***
(use-package flycheck
  :defer  t
  :init   (progn (with-no-warnings
                   (add-hook 'flycheck-mode-hook #'flycheck-checkbashisms-setup)
                   (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
                   (flycheck-clojure-setup)))
  :config (progn
            (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
            (global-flycheck-mode)))

;;;; *** VERSION CONTROL ***
(use-package magit
  :defer  t
  :config (progn
            (global-magit-file-mode))
  :bind   (("C-x g"   . magit-status)
           ("C-x M-g" . magit-dispatch-popup)
           ("C-c s"   . magit-stage-file)
           ("C-c C-s" . magit-unstage-file)
           ("C-c c"   . magit-commit-popup)
           ("C-c p"   . magit-push-popup)))

;;;; *** PARINFER ***
(use-package parinfer
  :defer  t
  :bind   (:map parinfer-mode-map
                ("C-p" . parinfer-toggle-mode))
  :config (progn
            (setq parinfer-extensions
                  '(defaults pretty-parens smart-yank paredit smart-tab)))
  :init   (progn
            (dolist (hook '(clojure-mode-hook
                            emacs-lisp-mode-hook
                            common-lisp-mode-hook
                            scheme-mode-hook
                            lisp-mode-hook
                            racket-mode-hook))
              (add-hook hook #'parinfer-mode))))



;;;; *** EMACS LISP ***
(defalias 'eb  #'eval-buffer)
(defalias 'er  #'eval-region)
(defalias 'ee  #'eval-expression)
(defalias 'elm #'emacs-lisp-mode)
(defalias 'lim #'lisp-interaction-mode)
(defalias 'eis #'elisp-index-search)

(defun el-lexical-bind ()
  "Sets `lexical-binding' on new elisp-files, disregarding *scratch*."
  (when (and (and (bobp) (eobp))
             (not (equal (buffer-name) "*scratch*")))
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (setq lexical-binding t)))

(add-hook 'emacs-lisp-mode-hook #'el-lexical-bind)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert initial-scratch-message)
  (lisp-interaction-mode))

;;;; *** COMMON LISP ***
(use-package slime
  :defer t
  :init  (progn
           (setq slime-lisp-implementations
                 '((sbcl ("/usr/local/bin/sbcl"))
                   (clisp ("/usr/local/bin/clisp")))
                 slime-contribs '(slime-fancy))))

;;;; *** SCHEME AND RACKET ***
(use-package scheme-mode
  :ensure f
  :mode   ("\\.\\(scm\\|ss\\)\\'" . scheme-mode))

(use-package geiser
  :commands geiser-mode
  :config   (progn
              (with-no-warnings
                (setq geiser-active-implementations '(guile)
                      geiser-default-implementation 'guile
                      geiser-mode-start-repl-p      t
                      geiser-debug-jump-to-debug-p  nil
                      geiser-guile-load-init-file-p t))))

(use-package racket-mode
  :defer  t
  :mode   ("\\.rkt[dl]?\\'" . racket-mode)
  :bind   (:map racket-mode-map
               ("C-x a" . racket-align))
  :config (progn
            (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
            (add-hook 'racket-mode-hook      #'parinfer-mode)
            (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)))

;;;; *** CLOJURE ***
(defun cider-connect-to-localhost ()
  "Connect with `cider-connect' to an already running REPL at localhost:7800"
  (interactive)
  (cider-connect "localhost" "7800"))

;; REFACTOR: this is way too sluggish and might even lock emacs down
;;           completely, especially compared to running lein repl on
;;           an eshell-instance of its own and just calling the
;;           ``cider-connect-to-localhost''-function.

(defun establish-cider-connection ()
  "Start Leiningen REPL from `eshell' with default settings localhost:7800.
Then proceed with `cider-connect' to connect into it with
`cider-connect-to-localhost'"
  (interactive)
  (eshell-command (format "lein repl :headless localhost :port 7800"))
  (cider-connect-to-localhost))

(defalias 'cctc #'cider-connect-to-localhost)
(defalias 'ecc  #'establish-cider-connection)

(use-package clojure-mode
  :defer t
  :mode  ("\\.\\(clj\\|boot\\|cljx\\|edn\\|cljs\\|cljs.hl\\)\\'" . clojure-mode))

(use-package cider
  :defer  t
  :after  clojure-mode
  :init   (setq cider-lein-parameters
                "repl :headless localhost :port 7800"
                cider-repl-pop-to-buffer-on-connect 'display-only
                cider-repl-result-prefix ";; → "
                cider-repl-display-help-banner nil)
  :config (progn
            (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
            (add-hook 'cider-mode-hook      #'eldoc-mode)))

(use-package clj-refactor
  :defer  t
  :after  cider-mode
  :config (progn
            (setq cljr-suppress-middleware-warnings t)
            (cljr-add-keybindings-with-prefix "C-c C-m")
            (add-hook 'clojure-mode-hook #'clj-refactor-mode)))

;;;; *** HASKELL ***

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode))

(use-package dante
  :after    haskell-mode
  :commands dante-mode
  :init     (progn
              (add-hook 'haskell-mode-hook 'dante-mode)
              (add-hook 'haskell-mode-hook 'flycheck-mode)))

;;;; *** STANDARD ML ***
(use-package sml-mode
  :defer  t
  :mode   ("\\.\\(sml\\|sig\\)\\'" . sml-mode)
  :config (progn
            (setq sml-program-name "mosml"
                  sml-default-arg "-P full")))

;;;; *** OCAML ***
(use-package tuareg
  :defer  t
  :mode   ("\\.ml[ily]?$" . tuareg-mode)
  :config (progn
            (setq tuareg-indent-align-with-first-arg t
                  tuareg-match-patterns-aligned t)))

;;;; *** YASNIPPETS ***

;; TODO: look into this, it seems popular and probably for a reason.
;;       only really used as a requirement for ``clj-refractor-mode'' iirc atm.

(use-package yasnippet
  :defer t
  :init (progn
          (with-no-warnings
            (add-hook 'clojure-mode-hook #'yas-minor-mode))))

(provide 'init-prog)
;;; init-prog.el ends here
