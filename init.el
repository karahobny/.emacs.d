;;; -*- lexical-binding: t; -*-
;;; init.el --- Unitialization file and gc-related config

;;; Commentary:
;;;            Initializes the separated configuration files from user-defined
;;;            configuration-folder.  Some garbage-collecting tricks to speedup
;;;            startup hopefully

;;; Code:
;; (package-initialize)
;;;; *** misc. functions ***
(require 'cl)

(defmacro fn (fn &rest args)
  `(funcall (quote ,fn) ,@args))

(defalias 'act #'interactive)

(defun kb>>b (β)
  "Convert kilobytes to bytes."
  (act)
  (* 1024 β))

(defun mb>>b (β)
  "Convert megabytes to bytes."
  (act)
  (* 1024 (fn kb>>b β)))


;;;; *** variables and init functions ***
(defconst user-config-folder
  "~/.emacs.d/init"
  "User's defined folder for configuration-files.")

(defconst user-custom-file
  (concat (file-name-as-directory user-emacs-directory) "custom.elc")
  "User defined location for Emacs' customization-file")

(defconst load-all-files-from-config-folder nil
  "Whether to initialize all .el/.elc-files from user's config folder or not.
It's recommended to initialize them separately for easier debugging and
testing.

Default is set to initialize the files separately (nil).")

(defconst user-config-files
  '(init-system
    init-multimedia
    init-prog
    init-gui
    init-modeline
    init-fira-code-ligatures)
  "List and order of the user's configuration files to initialize.
Checked only if LOAD-ALL-FILES-FROM-CONFIG-FOLDER is set to nil")

(defconst compile-user-config-folder t
  "Check if USER-CONFIG-FOLDER has files to be byte-compiled.")

(defun initialize-user-config-files ()
  "The money shot. Initializes either user's defined files in USER-CONFIG-FILES in
the specified order, or indiscriminately probes to #'require every .el/.elc file
from USER-CONFIG-FOLDER."
  (if (null load-all-files-from-config-folder)
      (dolist (file user-config-files)
        (require file))
    (dolist (file (directory-files user-config-folder t ".\\.elc?$"))
      (load-library file))))


;;;; *** garbage collection ***
(setq-default garbage-collection-messages nil)

(defvar file-name-handler-alist-orig file-name-handler-alist
  "Placeholder for original FILE-NAME-HANDLER-ALIST setting.")

(defun gc-eval-max-threshold ()
  "Set `gc-cons-threshold' to a maximum defined amount."
  (interactive)
  (setq gc-cons-threshold  104857600 ;; 100mb
        gc-cons-percentage 0.6))

(defun gc-eval-min-threshold ()
  "Set `gc-cons-threshold' to a minimum defined amount ."
  (interactive)
  (setq gc-cons-threshold  10485760    ;; 10mb
        gc-cons-percentage 0.1))

(defun gc-eval-at-startup ()
  "Garbage collection settings unique to Emacs' startup."
  (garbage-collect)
  (gc-eval-min-threshold)
  (setq file-name-handler-alist file-name-handler-alist-orig)
  (makunbound 'file-name-handler-alist-orig))


;;;; *** initialization ***
(let ((gc-cons-threshold  most-positive-fixnum))
  (setq inhibit-startup-screen  t
        file-name-handler-alist nil
        load-prefer-newer       t
        custom-file             user-custom-file)
  (load custom-file 'no-error 'no-message)

  (when (bound-and-true-p compile-user-config-folder)
    (byte-recompile-directory (expand-file-name user-config-folder) 0))

  (add-to-list 'load-path user-config-folder)
  (initialize-user-config-files))

(run-with-idle-timer 2 nil       #'gc-eval-at-startup)
(add-hook 'minibuffer-setup-hook #'gc-eval-max-threshold)
(add-hook 'minibuffer-exit-hook  #'gc-eval-min-threshold)
(add-hook 'focus-out-hook        #'garbage-collect)

(provide 'init)
;;; init.el ends here
