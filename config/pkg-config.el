;;; -*- lexical-binding: t -*-
;;; pkg-config.el --- package management
;;; Commentary:
;;;            The usual settings for MELPA and Paradox to replace the
;;;            regular package.el for browsing through repository.

;;; Code:
;; => package.el and melpa
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; => use-package

;; TODO: figure out how to better use this withut having to resort to
;;       such workarounds as (with-no-warnings) and (dolist (hook))
;;       instead of use-package's own :hook-property.

(progn
  (require 'use-package)
  (require 'diminish)
  (setq use-package-always-defer  t
        use-package-always-ensure t
        use-package-verbose       t))

;; => dired (stuck here for the meantime instead of init.el)
(use-package dired
  :ensure f
  :defer  t
  :config (progn
            (setq dired-use-ls-dired nil)))

;; => auto-compile
(use-package auto-compile
  :defer  t
  :init   (with-no-warnings
            (progn
              (auto-compile-on-load-mode)
              (auto-compile-on-save-mode)))
  :config (setq auto-compile-display-buffer   nil
                auto-compile-update-autoloads t))
;; => paradox

;; FIXME: `paradox-enable' isn't showing github stars when paradox is
;;        executed through `package-list-packages'.

(use-package paradox
  :defer    t
  :commands (paradox-upgrade-packages paradox-list-packages)
  :config   (with-no-warnings
              (progn
                (setq paradox-lines-per-entry 2
                      paradox-execute-asynchronously t)
                (paradox-enable)))
  :init     (progn
              (remove-hook 'paradox--report-buffer-print
                           #'paradox-after-execute-functions))
  :bind     ("C-x C-u" . paradox-upgrade-packages)
            ("C-x C-p" . paradox-list-packages))

(provide 'pkg-config)
;;; pkg-config.el ends here
