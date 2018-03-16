;;; lisp-config.el --- lisp and elisp
;;; Commentary:
;;;            SLIMEY BLIMEY GRIMEY

;;; Code:
;; => slime
(use-package slime
  :defer t
  :config
  (setq slime-lisp-implementations
        '((sbcl ("/usr/local/bin/sbcl"))
          (clisp ("/usr/local/bin/clisp"))))
  (setq slime-contribs '(slime-fancy)))

;; => initialize new emacs lisp files
(defun el-lexical-bind ()
  "Sets lexical-binding to t on new elisp-files, disregarding *scratch*."
  (when (and
          (and (bobp) (eobp))
          (not (equal (buffer-name) "*scratch*")))
    (insert ";;; -*- lexical-binding: t -*-\n")
    (setq lexical-binding t)))

(add-hook 'emacs-lisp-mode-hook #'el-lexical-bind)

;; => *scratch*
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert initial-scratch-message)
  (lisp-interaction-mode))

(provide 'lisp-config)
;;; lisp-config.el ends here
