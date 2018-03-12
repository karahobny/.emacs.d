;;; -*- lexical-binding: t -*-
;;; clj-config.el --- closure about clojure
;;; Commentary:
;;;            Full-blown Clojure IDE easily with CIDER,
;;;            clj-refactor etc.

;;; Code:

;; FIXME: `cider-jack-in' throws an error  [...] terminal width=0 is it emacs".
;;        `cider-connect' works flawlessy though, but it's more of a hassle.
;;        there has to be an easier way to do all this, but atleast i'm honing
;;        my elisp-skills.

(defun cider-connect-to-localhost ()
  "Connect with `cider-connect' to an already running REPL at localhost:7800"
  (interactive)
  (cider-connect "localhost" "7800"))

(defun establish-cider-connection ()
  "Start Leiningen REPL from `eshell' with default settings localhost:7800.
Then proceed with `cider-connect' to connect into it with `cider-connect-to-localhost'"
  (interactive)
  (eshell-command
   (format "lein repl :headless localhost :port 7800"))
  (cider-connect-to-localhost))

(use-package cider
  :defer  t
  :config (setq cider-lein-parameters
                "repl :headless :host localhost :port 7888"
                cider-repl-pop-to-buffer-on-connect 'display-only
                cider-repl-result-prefix ";; → "
                cider-repl-display-help-banner nil)
  :init   (progn
            (add-hook 'cider-repl-mode-hook
                      #'cider-company-enable-fuzzy-completion)
            (add-hook 'cider-repl-mode-hook #'eldoc-mode)
            (add-hook 'clojure-mode-hook    #'establish-cider-connection)))

(use-package clj-refactor
  :defer  t
  :config (progn
            (setq cljr-suppress-middleware-warnings t)
            (cljr-add-keybindings-with-prefix "C-c C-m"))
  :init   (add-hook 'clojure-mode-hook #'clj-refactor-mode))

(provide 'clj-config)
;;; clj-config.el ends here
