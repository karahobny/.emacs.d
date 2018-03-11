;;; -*- lexical-binding: t; -*-
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
  (setq use-package-always-defer  t
        use-package-always-ensure t
        use-package-verbose       t))

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
;; NOT SURE WHERE (PARADOX-ENABLE) GOES
(use-package paradox
  :demand   t
  :commands (paradox-upgrade-packages paradox-list-packages)
  :config   (with-no-warnings
              (progn
                (setq paradox-lines-per-entry 2
                      paradox-execute-asynchronously t)
                (paradox-enable)))
  :bind     ("C-x C-u" . paradox-upgrade-packages)
            ("C-x C-p" . paradox-list-packages))

(provide 'pkg-config)
;;; pkg-config.el ends here
