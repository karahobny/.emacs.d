;;; -*- lexical-binding: t; -*-
;;; init-system.el --- Emacs startup outside of init.el

;;; Commentary:
;;;            Package.el, MELPA and Paradox initialized.
;;;            Emacs-built-ins being loaded etc.

;;; Code:
;;;; PACKAGE MANAGEMENT
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require 'use-package)
(require 'diminish)
(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-verbose t)

(use-package paradox
  :defer t
  :commands (paradox-upgrade-packages paradox-list-packages)
  :config
  (with-no-warnings
    (setq paradox-lines-per-entry 1
          paradox-execute-asynchronously t)
    (paradox-enable))
  :init
  (remove-hook 'paradox--report-buffer-print #'paradox-after-execute-functions)
  :bind
  ("C-x C-u" . paradox-upgrade-packages)
  ("C-x C-p" . paradox-list-packages))

(use-package auto-compile
  :defer t
  :init
  (with-no-warnings
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode))
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-update-autoloads t))

;;;; BACKUPS / AUTOSAVES
(setq backup-directory-alist `(("." . "~/.emacs.d/backup"))
      backup-by-copying-when-linked t
      version-control t
      delete-old-versions t
      kept-old-versions 4
      kept-new-versions 4
      auto-save-default t
      auto-save-interval 200)

(defun my-backup-file-name (filename)
  "Backup FILENAME to replace the default one."
  (expand-file-name
   (concat "." (file-name-nondirectory filename) "~")
   (file-name-directory filename)))
(setq make-backup-file-name-function 'my-backup-file-name)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;;; ESHELL
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
  :defer t
  :config
  (add-hook 'eshell-banner-load-hook #'theo-eshell-banner)
  :bind
  (("C-c §" . eshell)
   ("C-x §" . eshell))
  :hook (eshelll-mode . eshell-bookmark-setup))

(use-package em-smart
  :ensure f
  :defer t
  :hook (eshell-mode . eshell-smart-initialize))

;;;; FILESYSTEM
(use-package dired
  :ensure f
  :defer t
  :config
  (setq dired-use-ls-dired   nil)
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(use-package neotree
  :defer t
  :config
  (setq neo-theme 'arrow)
  :bind
  ("C-x C-n" . neotree-toggle))

(provide 'init-system)
;;; init-system.el ends here
