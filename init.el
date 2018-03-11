;;; -*- lexical-binding: t; -*-
;;; init.el --- initialization file and gc-related config
;;; Commentary:
;;;            Initializes the separated configuration files from user-defined
;;;            configuration-folder.  Might considering switching to use-package
;;;            but migration takes a while and I didn't see any boost to my
;;;            startup time even though I deferred like crazy.

;;; Code:

;; commented out due to pkg-config.el handling package-initializing
;; (package-initialize)

(defconst user-config-folder
  "~/.emacs.d/config"
  "User's defined folder for configuration-files.")
(defconst user-custom-file
  (concat (file-name-as-directory user-emacs-directory) "custom.el")
  "User defined location for Emacs' customization-file")

(defvar load-all-files-from-config-folder nil
  "Whether to initialize all .el/.elc-files from user's config folder or not.
It's recommended to initialize them separately for easier debugging and
testing.

Default is set to initialize the files separately (nil).")

(defvar user-config-files nil
  "List of the user's configuration files to initialize.
Checked only if load-all-files-from-config-folder set to nil")

;; speedup tricks from hlissner, bling, reddit, sx etc.
;; TODO: prefer gc-cons-percentage

(defvar garbage-collect-max-num most-positive-fixnum
  "Maximum threshold for garbage collection.")
(defvar garbage-collect-min-num 800000
  "Minimum threshold for garbage collection.")
(defvar file-name-handler-alist-orig nil
  "Placeholder for original file-name-handler-alist setting.")
(setq file-name-handler-alist-orig file-name-handler-alist)

(progn  
  (setq inhibit-startup-screen t
        ;; garbage collection --- pre-startup
        gc-cons-threshold garbage-collect-max-num
        ;; empty the file-name-handler-alist
        file-name-handler-alist nil
        ;; loading the neccesary configuration files
        load-prefer-newer t
        load-all-files-from-config-folder nil
        user-config-files '("pkg-config"
                            "backup-config"
                            "keybindings"
                            "visual-config"
                            "mingus-config"
                            "eshell-config"
                            "doc-config"
                            "y-or-n-config"
                            "parinfer-config"
                            "c-config"
                            "erlang-config"
                            "lisp-config"
                            "scm-config"
                            "ml-config"
                            "company-config"
                            "my-helm-config"
                            "fly-config"
                            "undo-tree-config"
                            "browser-config"
                            "mode-line-config"
                            "fira-code-ligatures")))

;; system
(add-to-list 'load-path user-config-folder)
(byte-recompile-directory (expand-file-name user-config-folder) 0)

;; user configuration
;; => custom-file
(setq custom-file user-custom-file)
(load custom-file 'no-error 'no-message)

;; => separate configuration
;; REFACTOR: there are faster ways to do this.
;;           there simply has to be.
(if (null load-all-files-from-config-folder)
    (dolist (file user-config-files)
      (load-library file))
  (dolist (file (directory-files user-config-folder t ".\\.elc?$"))
    (load-library file)))

;; ==> workaround `bsd ls` not having --dired
(require 'dired)
(with-eval-after-load 'dired
  (setq dired-use-ls-dired nil))

;; garbage collection --- after startup
;; => reset garbage collection to reasonable defaults

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (setq gc-cons-threshold garbage-collect-min-num
;;                   file-name-handler-alist file-name-handler-alist-origin)
;;             (makunbound 'file-name-handler-alist-origin)))

;; i found this to be snappier at startup since gc is not set
;; immediately to low enough threshold to facilitate a garbage-collect
(run-with-idle-timer 5 nil
                     (lambda ()
                       (garbage-collect)
                       (setq gc-cons-threshold garbage-collect-min-num
                             file-name-handler-alist file-name-handler-alist-orig)
                       (makunbound 'file-name-handler-alist-origin)))

;; => minibuffer gc (opening one maxes out threshold and vice versa)
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq gc-cons-threshold garbage-collect-max-num)))
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (setq gc-cons-threshold garbage-collect-min-num)))

;; => garbage collect whenever focus is lost and/or idle
(add-hook 'focus-out-hook #'garbage-collect)

(provide 'init)
;;; init.el ends here
