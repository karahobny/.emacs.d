;;; eshell-config.el --- lispy shell to get used to
;;; Commentary:
;;;            Setting the prompt through eshell-prompt-extras.el,
;;;            modifying the banner and trying to get the Plan 9
;;;            -inspired smart display to display properly.

;;; Code:

(defun my-eshell-banner ()
  (setq eshell-banner-message
        (if (executable-find "theo")
            (concat (shell-command-to-string "theo")
                    "\n		~~Theo de Raadt\n\n\n")
          (concat "Welcome to the Emacs shell "
                  user-login-name "\n"))))

(use-package eshell
  :ensure f
  :defer  t
  :init   
  :config (with-no-warnings
            (progn
              (add-hook 'eshell-banner-load-hook #'my-eshell-banner)
              (add-hook 'eshell-mode-hook        #'eshell-bookmark-setup)))
  :bind ("C-x !" . eshell))

(use-package em-smart
  :defer  t
  :ensure f
  :hook   (eshell-mode . eshell-smart-initialize))

(provide 'eshell-config)
;;; eshell-config.el ends here
