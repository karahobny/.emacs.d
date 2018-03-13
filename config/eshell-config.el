;;; eshell-config.el --- lispy shell to get used to
;;; Commentary:
;;;            Setting the prompt through eshell-prompt-extras.el,
;;;            modifying the banner and trying to get the Plan 9
;;;            -inspired smart display to display properly.

;;; Code:

(defvar eshell-banner-message nil
  "Banner for Eshell to replace the default one.")

(defun theo-eshell-banner ()
  "If 'theo'-program found, uses it in the Eshell banner string.
Otherwise defaults to Eshell-default welcoming you."
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
              (add-hook 'eshell-banner-load-hook #'theo-eshell-banner)))
  :bind   (("C-c §" . eshell)
           ("C-x §" . eshell))
  :hook   (eshelll-mode . eshell-bookmark-setup))

(use-package em-smart
  :defer  t
  :ensure f
  :hook   (eshell-mode . eshell-smart-initialize))

(provide 'eshell-config)
;;; eshell-config.el ends here
