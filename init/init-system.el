;;; -*- lexical-binding: t; -*-
;;; init-system.el --- Emacs startup outside of init.el

;;; Commentary:
;;;            package.el, MELPA and Paradox initialized.
;;;            Emacs-built-ins being loaded etc.  (dired, eshell, whatever).
;;;            Might need to organize eshell configuration to its own init-file.

;;; Code:
;;;; *** package management ***
(require 'package)
(setq package-enable-at-startup  nil
      package--init-file-ensured t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; use-package initialization.
;; install it, if its not already installed and/or found.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-defer  t
      use-package-always-ensure t
      use-package-verbose       t)
(use-package diminish :ensure t :demand t)

(use-package paradox
  :defer  20
  :config (progn
            (setq paradox-lines-per-entry        1
                  paradox-execute-asynchronously t
                  paradox-spinner-type           'progress-bar-filled))
  :bind   (("C-x C-u" . paradox-upgrade-packages)
           ("C-x C-p" . paradox-list-packages)))


;;;; *** locale ***
(set-language-environment   "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system  'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system       'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


;;;; *** file management ***
(setq backup-directory-alist        `(("." . "~/.emacs.d/backup"))
      backup-by-copying-when-linked t
      version-control               t
      delete-old-versions           t
      kept-old-versions             4
      kept-new-versions             4
      auto-save-default             t
      auto-save-interval            200)

(defun my-backup-file-name (filename)
  "Backup FILENAME to replace the default one."
  (expand-file-name
   (concat "." (file-name-nondirectory filename) "~")
   (file-name-directory filename)))

(setq make-backup-file-name-function 'my-backup-file-name
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package auto-compile
  :demand t
  :config (progn
            (auto-compile-on-load-mode)
            (auto-compile-on-save-mode)
            (setq auto-compile-display-buffer   nil
                  auto-compile-update-autoloads t)))

(use-package dired
  :ensure f
  :defer  t
  :config (progn
            (when (eq system-type 'berkeley-unix)
              (setq dired-use-ls-dired nil))
            (use-package all-the-icons-dired
              :defer  t
              :hook   (dired-mode . all-the-icons-dired-mode))
            (use-package neotree
              :defer  t
              :config (setq neo-theme 'icons)
              :bind   ("C-x C-n" . neotree-toggle))))


;;;; *** eshell ***
(defvar eshell-banner-message nil
  "Banner for Eshell to replace the default one.")

(defun theo-eshell-banner ()
  "If 'theo'-program found, use it in the Eshell banner string.
Otherwise defaults to Eshell-default welcoming you."
  (setq eshell-banner-message
        (if (executable-find "theo")
            (concat (shell-command-to-string "theo")
                    "		~~Theo de Raadt\n\n")
          (concat "Welcome to the Emacs shell "
                  user-login-name "\n"))))

(defun eshell-mode-hook-functions ()
  "Functions/modes to apply specifically to Eshell-instance."
  (company-mode -1))

(use-package eshell
  :ensure f
  :defer  t
  :config (progn
            (add-hook 'eshell-banner-load-hook #'theo-eshell-banner)
            (add-hook 'eshell-mode-hook        #'eshell-mode-hook-functions)
            (use-package em-smart
              :ensure f
              :hook   (eshell-mode . eshell-smart-initialize))
            (use-package esh-opt
              :ensure f
              :config (progn
                        (autoload 'epe-theme-lambda "eshell-prompt-extras")
                        (with-no-warnings
                          (setq eshell-highlight-prompt nil
                                eshell-prompt-function  'epe-theme-lambda))))
            (use-package eshell-bookmark
              :hook (eshell-mode . eshell-bookmark-setup)))
  :bind   (("C-c §" . eshell)
           ("C-x §" . eshell)))

(provide 'init-system)
;;; init-system.el ends here
