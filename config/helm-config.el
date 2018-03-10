;;; helm-config.el --- helm keybindings mostly
;;; Commentary:

;;; Code:
(require 'helm)

;; => variables
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-M-x-fuzzy-match t
      helm-quick-update t)

;; => keybindings
(global-unset-key (kbd "M-x"))
(global-set-key   (kbd "M-x")     #'helm-M-x)
(global-set-key   (kbd "C-x C-m") #'helm-M-x)
(global-set-key   (kbd "C-c C-m") #'helm-M-x)
(global-set-key   (kbd "C-x C-f") #'helm-find-files)

(provide 'helm-config)
;;; helm-config ends here
