;;; -*- lexical-binding: t; -*-
;;; init.el --- Unitialization file and gc-related config

;;; Commentary:
;;;            Initializes the separated configuration files from user-defined
;;;            configuration-folder.  Some garbage-collecting tricks to speedup
;;;            startup hopefully

;;; Code:
;; (package-initialize)

;;;; ** VARIABLES AND INITIALIZATIVE FUNCTIONS **
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
  "List of the user's configuration files to initialize.
Checked only if load-all-files-from-config-folder set to nil")

(defconst compile-user-config-folder t
  "Check if user-config-folder has files to be byte-compiled.")

;; REFACTOR:
;;           ** dolist vs. cl-loop vs. mapc benchmarking. **
;;           wildly different results due to alignment of the stars or
;;           something as inexplicable. `dolist' and `cl-loop' seem
;;           to be the definite top contenders (also tried with dash.el's
;;           `-map'-function, but it seemed even more costly than `mapc'.

;;           saved the cl-loop-codesnippet for posterity's sake:
;;           (cl-loop for file
;;                    in user-config-files
;;                    collect (require file))

(defun initialize-user-config-files ()
  "The money shot. Initializes either user's defined `user-config-files' in that
order, or indiscriminately every .el/.elc file from `user-config-folder'."
  (if (null load-all-files-from-config-folder)
      (dolist (file user-config-files)
        (require file))
    (dolist (file (directory-files user-config-folder t ".\\.elc?$"))
      (load-library file))))

;;;; ** GARBAGE COLLECTION **
;; hlissner's doom-emacs gc speedup trix
(defconst gc-threshold-max 402653184
  "Maximum threshold for garbage collection.")
(defconst gc-threshold-min 16777216
  "Minimum threshold for garbage collection.")
(defconst gc-percentage-max 0.6
  "Maximum threshold for garbage collection.")
(defconst gc-percentage-min 0.1
  "Minimum threshold for garbage collection.")

(defvar file-name-handler-alist-orig file-name-handler-alist
  "Placeholder for original file-name-handler-alist setting.")

(defun gc-eval-max-threshold ()
  "Set `gc-cons-threshold' to maximum defined amount."
  (setq gc-cons-threshold  gc-threshold-max
        gc-cons-percentage gc-percentage-max))

(defun gc-eval-min-threshold ()
  "Set `gc-cons-threshold' to minimum defined amount."
  (setq gc-cons-threshold  gc-threshold-min
        gc-cons-percentage gc-percentage-min))

(defun gc-eval-at-startup ()
  "Garbage collection to handle at Emacs' startup."
  (garbage-collect)
  (gc-eval-min-threshold)
  (setq file-name-handler-alist file-name-handler-alist-orig)
  (makunbound 'file-name-handler-alist-orig))

;;;; ** INITIALIZATION **
(progn
  (gc-eval-max-threshold)
  (setq inhibit-startup-screen  t
        file-name-handler-alist nil
        load-prefer-newer       t
        custom-file             user-custom-file)
  (load custom-file 'no-error 'no-message)

  (when (bound-and-true-p compile-user-config-folder)
    (byte-recompile-directory (expand-file-name user-config-folder) 0))

  (add-to-list 'load-path user-config-folder)
  (initialize-user-config-files))

(run-with-idle-timer 5 nil       #'gc-eval-at-startup)
(add-hook 'minibuffer-setup-hook #'gc-eval-max-threshold)
(add-hook 'minibuffer-exit-hook  #'gc-eval-min-threshold)
(add-hook 'focus-out-hook        #'garbage-collect)

(provide 'init)
;;; init.el ends here
