;;; -*- lexical-binding: t; -*-
;;; visual-config.el --- all things aesthetic
;;; Commentary:
;;;            Line-numbers, riced scratch buffer, highlights etc.

;;; Code:
(global-visual-line-mode)
(show-paren-mode 1)
(setq custom-safe-themes t)

;; => theme
;;(load-theme 'xresources t)
(load-theme 'doom-one t)

;; => line numbers
(use-package nlinum
  :defer t
  :commands nlinum-mode
  :init  (setq nlinum-format "%4d")
  :bind  ("M-n" . nlinum-mode)
  :hook  (prog-mode))

;; => scratch message
(setq initial-scratch-message ";;
;;	 　　∧＿∧
;;	　 （　・∀・）　　　|　|      ＿＿＿＿＿＿＿＿＿＿＿＿＿
;;	　と　　　　）　 　 |　|    (  λ－ＣＡＬＣＵＬＵＳ！！  )
;;	　　 Ｙ　/ノ　　　 人       | ﾉ￣￣￣￣￣￣￣￣￣￣￣￣
;;	　　　 /　）　 　 < 　>_∧∩  '
;;	　 ＿/し'　／／. Ｖ｀Д´）/
;;	　（＿フ彡　　　　　 　　/
;;
\n")

;; => prettified symbols [TODO: add more]
(use-package prettify-symbols
  :defer  t
  :init   (global-prettify-symbols-mode)
  :config (setq prettify-symbols-alist
                '(("lambda" . 955)))
  :hook   (prog-mode))

;; => window-dividers
(set-face-attribute 'vertical-border
                     nil
                    :foreground "#222222")

;; => highligts
;; ==> highlight characters going over 80 char limit
;; FIXME: use-package's :hook not working for some reason.
(use-package whitespace
  :init  (with-no-warnings
           (dolist (hook
                    '(prog-mode-hook
                      emacs-lisp-mode-hook
                      lisp-mode-hook
                      scheme-mode-hook
                      text-mode-hook))
            (add-hook hook #'whitespace-mode)))
  :config (setq whitespace-line-column 82
                  whitespace-style       '(face lines-tail)))

;; ==> highlight keywords lke FIXME, TODO etc.

;; FIXME: prog-mode-hook used to be enough.
;;        nowadays i have to hook it separately to all
;;        major modes.

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))
(dolist (hook
         '(prog-mode-hook
           emacs-lisp-mode-hook
           scheme-mode-hook
           common-lisp-mode-hook
           clojure-mode-hook))
  (add-hook hook #'font-lock-comment-annotations))

(provide 'visual-config)
;;; visual-config.el ends here
1
