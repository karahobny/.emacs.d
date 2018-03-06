;;; visual-config.el --- all things aesthetic
;;; Commentary:
;;;            Line-numbers, riced scratch buffer, highlights etc.

;;; Code:
(global-visual-line-mode)
(show-paren-mode 1)
(load-theme 'xresources)

;; => nlinum (line numbers)
(require 'nlinum)
(setq nlinum-format "%4d ")
(global-set-key (kbd "M-n") #'nlinum-mode)
(add-hook 'prog-mode-hook   #'nlinum-mode)

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
(global-prettify-symbols-mode)
(setq prettify-symbols-alist
      '(("lambda" . 955)))

;; => window-dividers
(set-face-attribute 'vertical-border
                     nil
                    :foreground "#222222")

;; => highligts
;; ==> highlight characters going over 80 char limit
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; ==> highlight keywords lke FIXME, TODO etc.
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook #'font-lock-comment-annotations)

;; => pleasure of perusing
(add-hook 'doc-view-mode-hook
          (lambda ()
            (setq doc-view-fit-width-to-window t)
            (local-set-key (kbd "C-p")    #'doc-view-previous-page)
            (local-set-key (kbd "<up>")
                           #'doc-view-previous-line-or-previous-page)
            (local-set-key (kbd "C-n")    #'doc-view-next-page)
            (local-set-key (kbd "<down>")
                           #'doc-view-next-line-or-next-page)
            (local-set-key (kbd "C-+")    #'doc-view-enlarge)
            (local-set-key (kbd "C--")    #'doc-view-shrink)))

(provide 'visual-config)
;;; visual-config.el ends here
