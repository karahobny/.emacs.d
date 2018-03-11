;;; doc-config.el --- configuration for viewing epub/pdf etc. documents
;;; Commentary:
;;;             Ensuring nov is used automatically on epub-files and docview on pdf's.
;;;             Atleast for now.

;;; Code:
;; => nov
(use-package nov
  :defer t
  :mode  ("\\.epub\\'" . nov-mode)
  :bind  (:map nov-mode-map
              ("C-p" . nov-previous-document)
              ("C-n" . nov-next-document)
              ("p"   . nov-scroll-up)
              ("n"   . nov-scroll-down)))

;; => markdown
(use-package markdown-mode
  :defer  t
  :config (setq markdown-asymmetric-header t)
  :mode   (("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode)))

(provide 'doc-config)
;;; doc-config.el ends here
