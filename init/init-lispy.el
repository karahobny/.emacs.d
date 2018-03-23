;;; -*- lexical-binding: t; -*-
;;; init-lispy.el --- Initialize programming modes for Lisp-dialects

;;; Commentary:
;;;            SLIMEY BLIMEY GRIMEY

;;; Code:
;;;;  ** COMMON LISP **
(use-package slime
  :defer t
  :init
  (setq slime-lisp-implementations
        '((sbcl ("/usr/local/bin/sbcl"))
          (clisp ("/usr/local/bin/clisp")))
        slime-contribs '(slime-fancy)))

;;;; ** ELISP **
(defun el-lexical-bind ()
  "Sets lexical-binding to t on new elisp-files, disregarding *scratch*."
  (when (and
          (and (bobp) (eobp))
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

;;;; ** SCHEME **
(use-package geiser
  :defer t
  :init
  (with-no-warnings
    (setq geiser-active-implementations '(guile racket))))

;;;; ** CLOJURE **
(defun cider-connect-to-localhost ()
  "Connect with `cider-connect' to an already running REPL at localhost:7800"
  (interactive)
  (cider-connect "localhost" "7800"))

(defun establish-cider-connection ()
  "Start Leiningen REPL from `eshell' with default settings localhost:7800.
Then proceed with `cider-connect' to connect into it with
`cider-connect-to-localhost'"
  (interactive)
  (eshell-command
   (format "lein repl :headless localhost :port 7800"))
  (cider-connect-to-localhost))

(use-package cider
  :defer t
  :init
  (setq cider-lein-parameters
        "repl :headless localhost :port 7888"
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-result-prefix ";; → "
        cider-repl-display-help-banner nil)
  (with-no-warnings
    (add-hook 'cider-repl-mode-hook
            #'cider-company-enable-fuzzy-completion))
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

(use-package clj-refactor
  :defer t
  :init
  (setq cljr-suppress-middleware-warnings t)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'clojure-mode-hook #'clj-refactor-mode))

(provide 'init-lispy)
;;; init-lispy.el ends here
