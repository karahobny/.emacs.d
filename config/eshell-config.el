;;; eshell-config.el --- lispy shell to get used to
;;; Commentary:
;;;            Setting the prompt through eshell-prompt-extras.el,
;;;            modifying the banner and trying to get the Plan 9
;;;            -inspired smart display to display properly.

;;; Code:
(require 'eshell)

;; => keybindings
(global-set-key (kbd "C-x !") 'eshell)

;; => prompt
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

;; => bookmarks integration
(add-hook 'eshell-mode-hook 'eshell-bookmark-setup)

;; => banner
(add-hook 'eshell-banner-load-hook
          '(lambda ()
             (setq eshell-banner-message
                   (if (executable-find "theo")
                       (concat (shell-command-to-string "theo")
                               "\n		~~Theo de Raadt\n\n\n")
                     (concat "Welcome to the Emacs shell "
                             user-login-name "\n")))))

;; => smart display
(require 'em-smart)
(setq eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t)

(provide 'eshell-config)
;;; eshell-config.el ends here
