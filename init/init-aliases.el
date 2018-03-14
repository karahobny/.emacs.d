;;; -*- lexical-binding: t; -*-
;;; init-aliases.el --- Aliasing common functions into bite-sized koans

;;; Commentary:
;;;            Common functions aliased into two-three-maybe-even-four letters
;;;            max.

;;; Code:
(defalias 'eb #'eval-buffer)
(defalias 'er #'eval-region)
(defalias 'ee #'eval-expression)
(defalias 'elm #'emacs-lisp-mode)
(defalias 'lim #'lisp-interaction-mode)
(defalias 'eis #'elisp-index-search)
(defalias 'ecc #'establish-cider-connection)
(defalias 'cctc #'cider-connect-to-localhost)
(defalias 'list-buffers #'ibuffer)

(provide 'init-aliases)
;;; init-aliases.el ends here
