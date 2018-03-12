;;; -*- lexical-binding: t -*-
;;; mode-line-config.el --- prettifying the modeline
;;; Commentary:
;;;            Default EMACS' modeline is pretty uggo, so it needs a
;;;            huge overhaul, mostly abbreviating or hiding a lot of
;;;            unneccesary minor modes, but also major.  The modelines'
;;;            format doesn't need to hold too much information.

;;; Code:

;; REFACTOR: use use-package's :diminish-property instead of these
;;           macros. stuff cyphejor to use-package format too.

;; => abbreviating major modes with cyphejor.el
(use-package cyphejor
  :defer  t
  :init   (cyphejor-mode)
  :config (setq
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
                 ("clojure"         "ℂλⅉ")
                 ("geiser"          "Γ" :prefix)
                 ("REPL"            "ℝ" :postfix)
                 ("SML"             "ml")
                 ("tuareg"          "ocaml")
                 ("haskell"         "〉λ꞊")
                 ("python"          "py")
                 ("menu"            "")
                 ("mode"            "")
                 ("package"         "↓")
                 ("paradox"         "↓")
                 ("nov"             "ℕ")
                 ("eShell"          "ⅇσ")
                 ("shell"           "sh")
                 ("text"            "ξ")
                 ("wdired"          "↯δ"))))

;; => hiding/abbreviating minor modes with diminish.el
;; ==> diminshed modes need to be evaluated after load
;; source: Samuel Laurén's .emacs (stolen from a gist)
(defmacro diminish-all (&rest modes)
  "Hide minor MODES from the mode line."
  `(progn
     ,@(mapcar (lambda (m)
                 (cl-multiple-value-bind (file mode)
                     (if (listp m)
                         (values (car m) (cdr m))
                       (values m (intern (concat (symbol-name m) "-mode"))))
                   `(with-eval-after-load
                        ,(symbol-name file)
                      (diminish (quote ,mode)))))
               modes)))

(defmacro diminish-single (file mode &optional replacement)
  "Diminish a single MODE with optional REPLACEMENT name"
  `(with-eval-after-load ,(symbol-name file)
     (diminish (quote ,mode) ,replacement)))

(diminish-all company
              flycheck
              (geiser    . geiser-autodoc-mode)
              (helm-mode . helm-mode)
              (simple    . visual-line-mode)
              (slime     . slime-autodoc-mode)
              (yasnippet . yas-minor-mode)
              eldoc
              undo-tree
              view
              which-key
              whitespace
              clj-refactor)

(diminish 'isearch-mode " ⅈ ")

(diminish-single abbrev   abbrev-mode   " α ")
(diminish-single parinfer parinfer-mode " π ")
(diminish-single slime    slime-mode    " Σ ")
(diminish-single geiser   geiser-mode   " γ ")

;; powerline/airline
(use-package powerline)
(use-package airline-themes
  :demand  t
  :config  (with-no-warnings
             (progn
               (load-theme 'airline-doom-one t))))

(provide 'mode-line-config)
;;; mode-line-config.el ends here
