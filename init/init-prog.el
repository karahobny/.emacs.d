;;; -*- lexical-binding: t; -*-
;;; init-prog.el --- Code autocompletion, language-specific modes, syntax checking.

;;; Commentary:
;;;            Programming language-specific modes and their configuration.
;;;            Code (and pretty much everything else) is autocompleted with
;;;            company.el and syntax checking through flycheck.el.  Version
;;;            controlling through magit.el of course.

;;; Code:
;;;; *** productivity ***
(use-package company
  :diminish (company-mode)
  :bind     (:map company-active-map
                  ("<tab>" . company-complete)
                  ("M-i"   . company-complete)
                  ("C-n"   . company-select-next)
                  ("C-p"   . company-select-previous))
  :init     (progn
              (setq company-minimum-prefix-length     4
                    company-selection-wrap-around     t
                    company-show-numbers              nil
                    company-idle-delay                0
                    company-tooltip-limit             10
                    company-tooltip-minimum-width     30
                    company-tooltip-align-annotations t))
  :config   (add-to-list 'company-backends 'merlin-company-backend)
  :hook     (after-init . global-company-mode))

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
              (use-package helm-flycheck :demand t :after (helm))
              (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  :hook     (after-init . global-flycheck-mode))

(use-package magit
  :defer  t
  :config (global-magit-file-mode)
  :bind   (("C-x g"   . magit-status)
           ("C-x M-g" . magit-dispatch-popup)
           ("C-c s"   . magit-stage-file)
           ("C-c C-s" . magit-unstage-file)
           ("C-c c"   . magit-commit-popup)
           ("C-c p"   . magit-push-popup)))

(use-package eldoc
  :diminish (eldoc-mode . " ⅇδ ")
  :hook     ((emacs-lisp-mode
              clojure-mode
              ielm-mode
              lisp-interaction-mode)
             . eldoc-mode))

(use-package comment-dwim-2
  :defer t
  :bind  ("M-;" . comment-dwim-2))

(use-package commenter
  :defer t
  :init  (setq comment-style 'extra-line)
  :hook  (tuareg-mode
          . (lambda ()
              (setq-local commenter-config
                          '((single
                             . ((comment-start      . "(*")
                                (comment-end        . "*)")
                                (comment-start-skip . "\\(/(+\\|(\\*+\\)\\s *")))
                            (multi
                             . ((comment-start      . "(*")
                                (comment-end        . " *)")
                                (comment-start-skip . "(*")
                                (comment-end-skip   . "*)")
                                (comment-continue   . " * ")
                                (comment-padding    . " ")
                                (comment-multi-line . t)))))
              (commenter-setup))))

(use-package parinfer
  :diminish (parinfer-mode . " π ")
  :bind     (:map parinfer-mode-map
                  ("C-p" . parinfer-toggle-mode))
  :init     (setq parinfer-extensions
                  '(defaults pretty-parens smart-yank paredit smart-tab))
  :hook     ((clojure-mode
              emacs-lisp-mode
              common-lisp-mode
              scheme-mode
              lisp-mode
              racket-mode
              inferior-emacs-lisp-mode)
             . parinfer-mode))

;;;; *** repl / inferior-mode ***
(defun comint-repl-hook ()
  "Hook to enhance  `comint' as a proper buffer for repl & inferior-modes."
  (setq comint-scroll-to-bottom-on-input  t
        comint-scroll-to-bottom-on-output t
        comint-scroll-show-maximum-output t
        comint-move-point-for-output      t))

;;;; *** skelly bones ***
(use-package executable :defer t :init (setq executable-query  'function))
(use-package time-stamp :defer t :init (setq time-stamp-pattern nil))
(use-package copyright :hook (before-save . copyright-update))
(use-package skeleton :defer  t)


;;;; *** emacs lisp ***
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


;;;; *** common lisp ***
(use-package slime
  :defer    t
  :mode     ("\\.\\(lisp\\|lsp\\|cl\\)\\'" . common-lisp-mode)
  :diminish ((slime-mode         . " Σ ")
             (slime-autodoc-mode . " δ "))
  :init     (setq slime-lisp-implementations '((sbcl ("/usr/local/bin/sbcl")))
                  slime-contribs             '(slime-fancy)))

(use-package slime-company
  :defer  t
  :after  (slime company)
  :config (slime-setup '(slime-company)))


;;;; *** scheme / racket ***
(use-package scheme
  :ensure f
  :defer  t
  :mode   ("\\.\\(scm\\|ss\\)\\'" . scheme-mode))

(use-package geiser
  :defer    t
  :after    (scheme-mode)
  :defines  (geiser-active-implementations
             geiser-default-implementation
             geiser-mode-start-repl-p
             geiser-debug-jump-to-debug-p
             geiser-guile-load-init-file-p)
  :diminish ((geiser-mode         . " γ ")
             (geiser-autodoc-mode . " δ "))
  :init     (setq geiser-active-implementations '(guile)
                  geiser-default-implementation 'guile
                  geiser-mode-start-repl-p      t
                  geiser-debug-jump-to-debug-p  nil
                  geiser-guile-load-init-file-p t))

(use-package racket-mode
  :defer    t
  :diminish (racket-mode . " (λ) ")
  :mode     ("\\.rkt[dl]?\\'" . racket-mode)
  :bind     (:map racket-mode-map
                  ("C-x a" . racket-align))
  :hook     ((racket-mode racket-repl-mode) . racket-unicode-input-method-enable))

;;;; *** clojure ***
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
  :defer  t
  :mode   ("\\.\\(clj\\|boot\\|cljx\\|edn\\|cljs\\|cljs.hl\\)\\'" . clojure-mode))

(use-package clj-refactor
  :defer     t
  :functions (cljr-add-keybindings-with-prefix)
  :commands  (clj-refactor-mode)
  :diminish  (clj-refactor-mode . " cλjr ")
  :init      (setq cljr-suppress-middleware-warnings t)
  :config    (cljr-add-keybindings-with-prefix "C-c C-m")
  :hook      (clojure-mode . clj-refactor-mode))

(use-package cider
  :defer     t
  :functions (cider-company-enable-fuzzy-completion)
  :after     (clojure-mode)
  :commands  (cider-mode)
  :init      (setq cider-repl-pop-to-buffer-on-connect 'display-only
                   cider-repl-display-help-banner      nil
                   cider-repl-result-prefix            ";; ^ eval => "
                   cider-lein-parameters               "repl
                                                       :headless localhost
                                                       :port 7800")
  :hook      (cider-repl-mode . cider-company-enable-fuzzy-completion))


;;;; *** haskell ***
(use-package haskell-mode
  :defer t
  :mode  ("\\.hs\\'" . haskell-mode))

(use-package dante
  :defer    t
  :after    (haskell-mode)
  :diminish (dante-mode . "dmc")
  :hook     (haskell-mode . dante-mode))


;;;; *** ocaml ***
(use-package tuareg
  :defer       t
  :mode        (("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$"    . tuareg-mode))
  :bind        (:map tuareg-mode-map
                     ("M-RET"   . utop-eval-phrase)
                     ("M-;"     . comment-dwim-2)
                     ("C-c . b" . tuareg-insert-begin-form)
                     ("C-c . c" . tuareg-insert-class-form)
                     ("C-c . f" . tuareg-insert-for-form)
                     ("C-c . i" . tuareg-insert-if-form)
                     ("C-c . l" . tuareg-insert-let-form)
                     ("C-c . m" . tuareg-insert-match-form)
                     ("C-c . t" . tuareg-insert-try-form)
                     ("C-c . w" . tuareg-insert-while-form))
  :init        (setq tuareg-indent-align-with-first-arg            t
                     tuareg-match-patterns-aligned                 t
                     tuareg-use-abbrev-mode                        nil
                     tuareg-interactive-scroll-to-bottom-on-output t)
  :config      (progn
                 (use-package ocp-indent
                   :ensure f
                   :demand t)
                 (setq-local indent-line-function          'ocp-indent-line)
                 (setq-local indent-region-function        'ocp-indent-region)
                 (setq-local company-minimum-prefix-length 2))
  :custom-face (tuareg-font-double-colon-face
                ((t :foreground "dimgray"))))

(use-package merlin
  :ensure   f
  :defer    t
  :after    (tuareg-mode)
  :diminish (merlin-mode . "mahou shoujo")
  :bind     (:map merlin-mode-map
                  ("M-."        . merlin-locate)
                  ("M-,"        . merlin-pop-stack)
                  ("C-c C-o"    . merlin-occurrences)
                  ("C-c C-j"    . merlin-jump)
                  ("C-c C-i"    . merlin-locate-ident)
                  ("C-c <up>"   . merlin-type-enclosing-go-up)
                  ("C-c <down>" . merlin-type-enclosing-go-down))
  :init     (setq merlin-error-after-save    nil
                  merlin-command             'opam
                  merlin-completion-with-doc t)
  :hook     (tuareg-mode . merlin-mode))

(use-package utop
  :ensure   f
  :defer    t
  :after    (tuareg-mode)
  :diminish (utop-minor-mode)
  :init     (setq utop-command "utop -emacs")
  :hook     (tuareg-mode . utop-minor-mode))

(provide 'init-prog)
;;; init-prog.el ends here
