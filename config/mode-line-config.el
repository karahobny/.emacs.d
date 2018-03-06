;;; mode-line-config.el --- prettifying the modeline
;;; Commentary:
;;;            Default EMACS' modeline is pretty uggo, so it needs a
;;;            huge overhaul, mostly abbreviating or hiding a lot of
;;;            unneccesary minor modes, but also major.  The modelines'
;;;            format doesn't need to hold too much information.

;;; Code:
;; => format

;; (defun simple-mode-line-render (left right)
;;   "Return a string of `window-width' length containing LEFT and RIGHT aligned."
;;   (let* ((available-width (- (window-total-width) (length left) 2)))
;;     (format (format " %%s %%%ds " available-width) left right)))
;; (setq mode-line-format
;;       '((:eval (simple-mode-line-render
;;                 ;; left
;;                 (format-mode-line
;;                  '("  "
;;                    mode-line-buffer-identification
;;                    " [%*]"))
;;                 ;; right
;;                 (format-mode-line
;;                  '("  [%m]" mode-line-misc-info "  "))))))

(setq-default mode-line-format
      (list
       "%e"
       "   "
       mode-line-client
       mode-line-frame-identification
       mode-line-buffer-identification
       "  "
       mode-line-modes
       mode-line-misc-info
       mode-line-end-spaces))

;; => abbreviating major modes with cyphejor.el
(require 'cyphejor)
(setq
 cyphejor-rules
 '(:upcase
   ("bookmark"        "→")
   ("buffer"          "β")
   ("diff"            "Δ")
   ("dired"           "δ")
   ("emacs"           "ε")
   ("inferior"        "i" :prefix)
   ("interaction"     "i" :prefix)
   ("interactive"     "i" :prefix)
   ("Emacs-Lisp"      "ελ")
   ("lisp"            "λ")
   ("scheme"          "λ")
   ("clojure"         "cλj")
   ("geiser"          "Γ" :prefix)
   ("SML"             "ml")
   ("tuareg"          "ocaml")
   ("haskell"         "〉λ꞊")
   ("python"          "py")
   ("menu"            "")
   ("mode"            "")
   ("package"         "↓")
   ("paradox"         "↓")
   
   ;; TODO: fix eshell-mode (shown as E)
   ("e"               "esh")
   ("shell"           "sh")
   ("text"            "ξ")
   ("wdired"          "↯δ")))

;; => hiding/abbreviating minor modes with diminish.el
(require 'diminish)
;; ==> diminshed modes need to be evaluated after load
(with-eval-after-load 'abbrev
  (diminish 'abbrev-mode "α"))
(with-eval-after-load 'parinfer
  (diminish 'parinfer-mode "π"))
(with-eval-after-load 'company
  (diminish 'company-mode))
(with-eval-after-load 'flycheck
  (diminish 'flycheck-mode))
(with-eval-after-load 'undo-tree
  (diminish 'undo-tree-mode))
(with-eval-after-load 'slime
  (diminish 'slime-mode "Σ")
  (diminish 'slime-autodoc-mode))
(with-eval-after-load 'simple
  (diminish 'visual-line-mode))
(with-eval-after-load 'whitespace
  (diminish 'whitespace-mode))
(with-eval-after-load 'geiser
  (diminish 'geiser-autodoc-mode))
(with-eval-after-load 'isearch-mode
  (diminish 'isearch-mode "ⅈ"))
(with-eval-after-load 'view
  (diminish 'view-mode))

(provide 'mode-line-config)
;;; mode-line-config.el ends here
