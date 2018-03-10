;;; my-helm-config.el --- helm keybindings mostly
;;; Commentary:
;;;            Funnily enough there already exists a `helm-config` file to be
;;;            required apparently, so I had to act coy in naming this and go
;;;            with the usual my-kebab-case-shebang.
;;;            Otherwise this really just concerns helm that I'm still trying
;;;            to learn how to utilize it to it's full potential.

;;; Code:
(require 'helm)
(require 'helm-config)
(helm-mode 1)

;; => variables
(setq helm-split-window-in-side-p           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line        t)

;; => keybindings
(global-unset-key (kbd "M-x"))
(global-set-key   (kbd "M-x")     #'helm-M-x)
(global-set-key   (kbd "C-x C-m") #'helm-M-x)
(global-set-key   (kbd "C-c C-m") #'helm-M-x)
(global-set-key   (kbd "C-x C-f") #'helm-find-files)

(provide 'my-helm-config)
;;; my-helm-config.el ends here
