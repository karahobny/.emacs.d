;;; -*- lexical-binding: t -*-
;;; scm-config.el --- scheme-related configuration
;;; Commentary:
;;;            There really is no need for much configuration when
;;;            it comes to scheme-lang, since it is widely supported
;;;            by default in Emacs.

;;; Code:
(defun geiser-eval-region-or-last-sexp ()
  "Call `geiser-eval-region' or `geiser-eval-last-sexp'.
Depending on whether or not a region is selected."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (geiser-eval-region (point) (mark))
    (geiser-eval-last-sexp)))

(use-package geiser
  :defer  t
  :config (setq geiser-active-implementations '(guile
                                                racket)))
  
(provide 'scm-config)
;;; scm-config.el ends here
