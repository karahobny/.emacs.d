;;; -*- lexical-binding: t -*-
;;; init.el --- initialization file and gc-related config
;;; Commentary:
;;;            Initializes the separated configuration files from user-defined
;;;            configuration-folder.  Might considering switching to use-package
;;;            but migration takes a while and I didn't see any boost to my
;;;            startup time even though I deferred like crazy.

;;; Code:

;; commented out due to pkg-config.el handling package-initializing
;; (package-initialize)

;;;; VARIABLES AND INITIALIZATION FUNCTION

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

(defvar compile-user-config-folder nil
  "Check if user-config-folder has files to be byte-compiled.")
(setq   compile-user-config-folder t)

;; REFACTOR: there are faster ways to do this, probably.
;;           there simply has to be. (TODO: benchmark dolist vs mapc(ar))

(defun initialize-user-config-files ()
  "The money shot. Initializes either user's defined `user-config-files' in that
order, or indiscriminately every .el/.elc file from `user-config-folder'."
  (if (null load-all-files-from-config-folder)
      (dolist (file user-config-files)
        (load-library file))
    (dolist (file (directory-files user-config-folder t ".\\.elc?$"))
      (load-library file))))

;; speedup tricks from hlissner, bling, reddit, sx etc.
;; TODO: prefer gc-cons-percentage

(defvar garbage-collect-max-num most-positive-fixnum
  "Maximum threshold for garbage collection.")
(defvar garbage-collect-min-num 800000
  "Minimum threshold for garbage collection.")
(defvar file-name-handler-alist-orig nil
  "Placeholder for original file-name-handler-alist setting.")
(setq file-name-handler-alist-orig file-name-handler-alist)

;;;; INITIALIZATION
;; => variables
(progn  
  (setq inhibit-startup-screen t
        ;; garbage collection --- pre-startup
        gc-cons-threshold garbage-collect-max-num
        ;; empty the file-name-handler-alist
        file-name-handler-alist nil
        ;; loading the neccesary configuration files
        load-prefer-newer t
        user-config-files '("pkg-config"
                            "backup-config"
                            "keybindings"
                            "visual-config"
                            "mingus-config"
                            "eshell-config"
                            "doc-config"
                            "parinfer-config"
                            "c-config"
                            "erlang-config"
                            "lisp-config"
                            "clj-config"
                            "scm-config"
                            "ml-config"
                            "company-config"
                            "my-helm-config"
                            "fly-config"
                            "undo-tree-config"
                            "yas-config"
                            "browser-config"
                            "alias-config"
                            "mode-line-config"
                            "fira-code-ligatures")))

;; => loading config-files
(add-to-list 'load-path user-config-folder)
(when (bound-and-true-p compile-user-config-folder)
  (byte-recompile-directory (expand-file-name user-config-folder) 0))
(setq custom-file user-custom-file)
(load custom-file 'no-error 'no-message)
(initialize-user-config-files)

;;;; GARBAGE COLLECTION
;; => functions for setting current gc-state
(defun gc-eval-at-startup ()
  "Garbage collection to handle at Emacs' startup."
  (garbage-collect)
  (setq gc-cons-threshold garbage-collect-min-num
        file-name-handler-alist file-name-handler-alist-orig)
  (makunbound 'file-name-handler-alist-orig))

(defun gc-eval-max-threshold ()
  "Set `gc-cons-threshold' to maximum defined amount."
  (setq gc-cons-threshold garbage-collect-max-num))
(defun gc-eval-min-threshold ()
  "Set `gc-cons-threshold' to minimum defined amount."
  (setq gc-cons-threshold garbage-collect-min-num))

;; => hooks
;; (add-hook 'after-init-hook #'gc-eval-at-startup
(progn
  (run-with-idle-timer 5 nil       #'gc-eval-at-startup)
  (add-hook 'minibuffer-setup-hook #'gc-eval-max-threshold)
  (add-hook 'minibuffer-exit-hook  #'gc-eval-min-threshold)
  (add-hook 'focus-out-hook        #'garbage-collect))

(provide 'init)
;;; init.el ends here
