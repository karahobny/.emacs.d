;;; init.el --- initialization file.
;;; Commentary:
;;;            initializes the separated configuration files from
;;;            user-defined configuration-folder.

;;; Code:

;; TODO: check out use-package.el
;; commented out due to pkg-config.el handling package-initializing
;; (package-initialize)

(defvar user-config-folder "~/.emacs.d/config"
  "User's defined folder for configuration-files.")

(defvar load-all-files-from-config-folder nil
  "Whether to initialize .el/.elc-files from user's config folder or not.
It's recommended to initialize them separately for easier debugging and
testing.

Default is set to initialize the files separately.")

(defvar user-config-files nil
  "List of the user's configuration files to initialize.
Checked only if load-all-files-from-config-folder set to nil")

;; turn up the memory threshold to speed up startup
;; source hlissner/doom-emacs
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq load-all-files-from-config-folder nil
      user-config-files '("pkg-config"
                          "backup-config"
                          "keybindings"
                          "visual-config"
                          "mingus-config"
                          "y-or-n-config"
                          "parinfer-config"
                          "c-config"
                          "erlang-config"
                          "lisp-config"
                          "scm-config"
                          "ml-config"
                          "company-config"
                          "fly-config"
                          "undo-tree-config"
                          "browser-config"
                          "mode-line-config"
                          "fira-code-ligatures"))

;; system
(add-to-list 'load-path user-config-folder)
;; user configuration
;; => custom-file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
;; => separate configuration
(if (null load-all-files-from-config-folder)
    (mapc 'load-library user-config-files)
  (mapc 'load-library (file-expand-wildcards
                       (concat user-config-folder "/*.el*"))))

;; reset garbage collection to reasonable defaults
(add-hook 'emacs-startup-hook
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(provide 'init)
;;; init.el ends here
