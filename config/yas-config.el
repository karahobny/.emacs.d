;;; -*- lexical-binding: t; -*-
;;; yas-config.el --- yasnippets
;;; Commentary:
;;;            TODO: make a general configuration file for
;;;            packages like this. 

;;; Code:
(use-package yasnippet
  :diminish yas-minor-mode
  :init     (progn
              (add-hook 'clojure-mode-hook #'yas-minor-mode)))

(provide 'yas-config)
;;; yas-config.el ends here
