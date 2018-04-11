;;; -*- lexical-binding: t; -*-
;;; init-prog.el --- Code autocompletion, language-specific modes, syntax checking.

;;; Commentary:
;;;            Programming language-specific modes and their configuration.
;;;            Code (and pretty much everything else) is autocompleted with
;;;            company.el and syntax checking through flycheck.el.  Version
;;             controlling through magit.el of course.

;;; Code:
;;;; *** COMPANY ***
(use-package company
  :diminish (company-mode)
  :bind     (:map company-active-map
                  ("<tab>" . company-complete)
                  ("M-i"   . company-complete)
                  ("C-n"   . company-select-next)
                  ("C-p"   . company-select-previous))
  :config   (progn
              (setq company-minimum-prefix-length     4
                    company-selection-wrap-around     t
                    company-show-numbers              nil
                    company-idle-delay                0
                    company-tooltip-limit             10
                    company-tooltip-minimum-width     30
                    company-tooltip-align-annotations t)
              (add-to-list 'company-backends
                           'merlin-company-backend))
  :hook     (after-init . global-company-mode))


;;;; *** FLYCHECK ***
(use-package flycheck
  :commands (flycheck-mode global-flycheck-mode)
  :diminish (flycheck-mode)
  :bind     (:map flycheck-mode-map
                  ("C-c ! h" . helm-flycheck))
  :init     (progn
              (flycheck-clojure-setup)
              (flycheck-ocaml-setup)
              (flycheck-checkbashisms-setup)
              (flycheck-haskell-setup))
  :config   (progn
              (use-package helm-flycheck :after helm)
              (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
              (global-flycheck-mode))
  :hook     (after-init . global-flycheck-mode))


;;;; *** ELDOC ***
(use-package eldoc
  :diminish (eldoc-mode . " ⅇδ ")
  :hook     ((emacs-lisp-mode
              clojure-mode
              ielm-mode
              lisp-interaction-mode)
             . eldoc-mode))


;;;; *** VERSION CONTROL ***
(use-package magit
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
  :diminish (parinfer-mode . " π ")
  :bind     (:map parinfer-mode-map
                  ("C-p" . parinfer-toggle-mode))
  :config   (progn
              (setq parinfer-extensions
                    '(defaults pretty-parens smart-yank paredit smart-tab)))
  :hook     ((clojure-mode
              emacs-lisp-mode
              common-lisp-mode
              scheme-mode
              lisp-mode
              racket-mode)
             . parinfer-mode))


;;;; *** EMACS LISP ***
(defalias 'eb  #'eval-buffer)
(defalias 'er  #'eval-region)
(defalias 'ee  #'eval-expression)
(defalias 'elm #'emacs-lisp-mode)
(defalias 'lim #'lisp-interaction-mode)
(defalias 'eis #'elisp-index-search)

(defun el-lexical-bind ()
  "Set `lexical-binding' on new elisp-files, disregarding *scratch*."
  (when (and (and (bobp) (eobp))
             (not (equal (buffer-name) "*scratch*")))
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (setq lexical-binding t)))

(add-hook 'emacs-lisp-mode-hook #'el-lexical-bind)

(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert initial-scratch-message)
  (lisp-interaction-mode))


;;;; *** COMMON LISP ***
(use-package slime
  :defer    t
  :diminish (slime-mode . " Σ ")
  :config   (progn
              (setq slime-lisp-implementations  '((sbcl ("/usr/local/bin/sbcl")))
                    slime-contribs              '(slime-fancy))
              (use-package slime-company
                :demand t
                :init   (slime-setup '(slime-company)))))


;;;; *** SCHEME AND RACKET ***
(use-package scheme-mode
  :ensure f
  :mode   ("\\.\\(scm\\|ss\\)\\'" . scheme-mode))

(use-package geiser
  :defines  (geiser-active-implementations
             geiser-default-implementation
             geiser-mode-start-repl-p
             geiser-debug-jump-to-debug-p
             geiser-guile-load-init-file-p)
  :diminish (geiser-mode . " γ ") (geiser-autodoc-mode . " δ ")
  :config   (progn
              (setq geiser-active-implementations '(guile)
                    geiser-default-implementation 'guile
                    geiser-mode-start-repl-p      t
                    geiser-debug-jump-to-debug-p  nil
                    geiser-guile-load-init-file-p t)))

(use-package racket-mode
  :diminish (racket-mode . " (λ) ")
  :mode     ("\\.rkt[dl]?\\'" . racket-mode)
  :bind     (:map racket-mode-map
                  ("C-x a" . racket-align))
  :hook     ((racket-mode racket-repl-mode) . racket-unicode-input-method-enable))

;;;; *** CLOJURE ***
(defun cider-connect-to-localhost ()
  "Connect with `cider-connect' to an already running REPL at localhost:7800."
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
  :mode   ("\\.\\(clj\\|boot\\|cljx\\|edn\\|cljs\\|cljs.hl\\)\\'" . clojure-mode)
  :config (progn
            (use-package yasnippet
              :diminish yas-minor-mode
              :hook     (clojure-mode . yas-minor-mode))
            (use-package clj-refactor
              :functions cljr-add-keybindings-with-prefix
              :diminish  clj-refactor-mode
              :config    (progn
                           (setq cljr-suppress-middleware-warnings t)
                           (cljr-add-keybindings-with-prefix "C-c C-m"))
              :hook      (clojure-mode . clj-refactor-mode))))

(use-package cider
  :functions cider-company-enable-fuzzy-completion
  :after     clojure-mode
  :config    (progn
               (setq cider-lein-parameters
                     "repl :headless localhost :port 7800"
                     cider-repl-pop-to-buffer-on-connect 'display-only
                     cider-repl-result-prefix ";; => "
                     cider-repl-display-help-banner nil))
  :hook      (cider-repl-mode . cider-company-enable-fuzzy-completion))


;;;; *** HASKELL ***
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode))

(use-package dante
  :after    haskell-mode
  :diminish (dante-mode . "dmc")
  :hook     (haskell-mode . dante-mode))


;;;; *** STANDARD ML ***
(use-package sml-mode
  :defer  t
  :mode   ("\\.\\(sml\\|sig\\)\\'" . sml-mode)
  :config (setq sml-program-name "mosml"
                  sml-default-arg  "-P full"))


;;;; *** OCAML ***
(use-package tuareg
  :defer       t
  :mode        (("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$"    . tuareg-mode))
  :bind        (:map tuareg-mode-map
                     ("M-;"   . tuareg-comment-dwim)
                     ("C-c B" . tuareg-insert-begin-form)
                     ("C-c C" . tuareg-insert-class-form)
                     ("C-c F" . tuareg-insert-for-form)
                     ("C-c I" . tuareg-insert-if-form)
                     ("C-c L" . tuareg-insert-let-form)
                     ("C-c M" . tuareg-insert-match-form)
                     ("C-c T" . tuareg-insert-try-form)
                     ("C-c W" . tuareg-insert-while-form))
  :config      (progn
                 (setq tuareg-indent-align-with-first-arg            t
                       tuareg-match-patterns-aligned                 t
                       tuareg-use-abbrev-mode                        nil
                       tuareg-interactive-scroll-to-bottom-on-output t)
                 (use-package ocp-indent))
  :custom-face (tuareg-font-double-colon-face ((t :foreground "dimgray"))))

(use-package merlin
  :diminish (merlin-mode . "mahou shoujo")
  :bind     (:map merlin-mode-map
                  ("M-."        . merlin-locate)
                  ("M-,"        . merlin-pop-stack)
                  ("C-c C-o"    . merlin-occurrences)
                  ("C-c C-j"    . merlin-jump)
                  ("C-c C-i"    . merlin-locate-ident)
                  ("C-c <up>"   . merlin-type-enclosing-go-up)
                  ("C-c <down>" . merlin-type-enclosing-go-down))
  :config   (setq merlin-error-after-save    nil
                  merlin-command             'opam
                  merlin-completion-with-doc t)
  :hook     (tuareg-mode . merlin-mode))

(use-package utop
  :diminish utop-minor-mode
  :config   (setq utop-command "opam config exec -- utop -emacs")
  :hook     (tuareg-mode . utop-minor-mode))

(provide 'init-prog)
;;; init-prog.el ends here
