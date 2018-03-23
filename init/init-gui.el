;;; -*- lexical-binding: t; -*-
;;; init-gui.el --- prettifying Emacs piece-by-piece

;;; Commentary:
;;;            Default EMACS' modeline is pretty uggo, so it needs a
;;;            huge overhaul, mostly abbreviating or hiding a lot of
;;;            unneccesary minor modes, but also major. I went with
;;;            spaceline, since I am a sucker for pretty, shiny things.

;;; Code:
;;;; ** USER THEMES **
(setq custom-safe-themes t)
(use-package all-the-icons)

;;;; ** BUFFERS **
(global-visual-line-mode t)
(show-paren-mode         t)

(global-set-key (kbd "C-<right>") #'next-buffer)
(global-set-key (kbd "C-<left>")  #'previous-buffer)

(global-set-key (kbd "<home>") #'beginning-of-line)
(global-set-key (kbd "<end>")  #'end-of-line)
(global-set-key (kbd "C-c g")  #'goto-line)

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

(use-package nlinum
  :defer t
  :init
  (setq nlinum-format "%4d ")
  :config
  (add-hook 'prog-mode #'nlinum-mode)
  :bind
  ("M-n" . nlinum-mode))

(use-package prog-mode
  :ensure f
  :defer t
  :commands
  (prettify-symbols-mode global-prettify-symbols-mode)
  :init
  (setq prettify-symbols-alist '(("lambda" . 955)))
  (add-hook 'prog-mode #'prettify-symbols-mode)
  :config
  (global-prettify-symbols-mode))

(use-package browse-url
  :defer t
  :init
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program  "chrome"))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;;; ** WINDOWS / LAYOUTS
(winner-mode t)

(use-package ace-window
  :defer t
  :bind
  ("M-o" . ace-window))

(defvar win-horiz-resize-num 15
  "The amount of delta columns to shrink and/or enlarge a window horizontally")
(defvar win-vert-resize-num 5
  "The amount of delta columns to shrink and/or enlarge a window vertically")

(defun my-shrink-win-horiz ()
  "Shrink window horizontally by `win-horiz-resize-num'"
  (interactive)
  (shrink-window-horizontally win-horiz-resize-num))
(defun my-enlarge-win-horiz ()
  "Enlarge window horizontally by `win-horiz-resize-num'"
  (interactive)
  (enlarge-window-horizontally win-horiz-resize-num))
(defun my-shrink-win-vert ()
  "Shrink window vertically by `win-vert-resize-num'"
  (interactive)
  (shrink-window win-vert-resize-num))
(defun my-enlarge-win-vert ()
  "Enlarge window vertically by `win-vert-resize-num'"
  (interactive)
  (enlarge-window win-vert-resize-num))

(defun my-split-win-vert ()
  "Splits window vertically and then focuses on it."
  (interactive)
  (split-window-vertically)
  (other-window 1))
(defun my-split-win-horiz ()
  "Splits window vertically and then focuses on it."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(global-set-key (kbd "C-x 2") #'my-split-win-vert)
(global-set-key (kbd "C-x 3") #'my-split-win-horiz)
(global-set-key (kbd "C-c h") #'my-shrink-win-vert)
(global-set-key (kbd "C-c j") #'my-shrink-win-horiz)
(global-set-key (kbd "C-c k") #'my-enlarge-win-horiz)
(global-set-key (kbd "C-c l") #'my-enlarge-win-vert)

;;;; ** HELM **
(use-package helm
  :defer t
  :init
  (require 'helm-config)
  (global-unset-key (kbd "M-x"))
  (setq helm-split-window-inside-p        t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line    t)
  :config
  (helm-mode)
  :bind
  (("M-x"     . helm-M-x)
   ("C-x C-m" . helm-M-x)
   ("C-c C-m" . helm-M-x)
   ("C-x C-f" . helm-find-files)))

(use-package helm-google
  :defer t
  :config
  (setq helm-google-idle-delay 1
        helm-google-actions
        '(("Browse URL with EWW"             . (lambda (candidate)
                                                 (eww-browse-url candidate)))
          ("Copy URL to clipboard"           . (lambda (candidate)
                                                 (kill-new       candidate)))
          ("Browse URL with default program" . browse-url)))
  :bind
  ("C-h C--" . helm-google))

;;;; ** USER INPUT **
(use-package tmm
  :defer t
  :bind
  ("C-c b" . tmm-menubar))

(use-package which-key
  :defer t
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

(use-package undo-tree
  :defer t
  :config
  (with-no-warnings
    (global-undo-tree-mode))
  :bind
  (("C-z"   . undo-tree-undo)
   ("C-r"   . undo-tree-redo)
   ("C-Z"   . undo-tree-redo)
   ("C-c v" . undo-tree-visualize)))

(use-package comment-dwim-2
  :defer t
  :bind
  ("M-;" . comment-dwim-2))

(use-package multiple-cursors
  :disabled
  :defer t
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  ("M-<mouse-1>" . mc/add-cursor-on-click))

(use-package smooth-scrolling
  :defer t
  :init
  (smooth-scrolling-mode t))

(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(defun kill-region-or-word ()
  "Call `kill-region' or `backward-kill-word'.
Depending on whether or not a region is selected."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (kill-region (point) (mark))
    (backward-kill-word 1)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "RET")     #'newline-and-indent)
(global-set-key (kbd "C-a")     #'smart-line-beginning)
(global-set-key (kbd "C-<tab>") "\C-q\t")

(global-set-key (kbd "C-w")   #'kill-region-or-word)
(global-set-key (kbd "C-c e") #'eval-and-replace)

;;;; ** HIGHLIGHTS **
(defvar column-char-limit 82
  "Highlight characters going over the specified amount of columns/characters.")

(defvar char-limited-modes
  '(prog-mode-hook
    emacs-lisp-mode-hook
    lisp-mode-hook
    common-lisp-mode-hook
    scheme-mode-hook
    clojure-mode-hook
    text-mode-hook)
  "List of programming hooks to apply whitespace-mode and thus column character limit to.")

(use-package whitespace
  :ensure nil
  :defer t
  :config
  (setq whitespace-line-column column-char-limit
        whitespace-style '(face lines-tail))
  (dolist (mode-str char-limited-modes)
    (add-hook mode-str #'whitespace-mode)))

(defvar font-annotated-modes
  '(prog-mode-hook
    emacs-lisp-mode-hook
    lisp-mode-hook
    common-lisp-mode-hook
    scheme-mode-hook
    clojure-mode-hook)
  "List of programming hooks to apply `font-lock-comment-annotations' to.")

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(dolist (mode-str font-annotated-modes)
  (add-hook mode-str #'font-lock-comment-annotations))

;;;; ** MODELINE **
(use-package anzu
  :defer t
  :config
  (global-anzu-mode t))

;; mostly meaningless due to spaceline, but some of these are still needed
;; and most are kept just in case
(use-package cyphejor
  :defer t
  :init
  (setq cyphejor-rules
        '(:downcase
          ("exwm"            "ⅇ𝕩")
          ("apropos"         "???")
          ("bookmark"        "→")
          ("messages"        "msg")
          ("buffer"          "")
          ("help"            "???")
          ("ibuffer"         "β")
          ("compilation"     "!!!")
          ("compiling"       "...")
          ("custom"          "custom")
          ("diff"            "diff")
          ("dired"           "dir")
          ("helm"            "")
          ("esup"            "GOTTA GO FAST")
          ("emacs"           "ⅇ")
          ("inferior"        "𝕚" :prefix)
          ("interaction"     "𝕚" :prefix)
          ("interactive"     "𝕚" :prefix)
          ("emacs-lisp"      "ⅇλ")
          ("suggest"         "Σⅇλ")
          ("lisp"            "λ")
          ("scheme"          "λ")
          ("clojure"         "𝕔λⅉ")
          ("geiser"          "γ")
          ("SML"             "sml")
          ("tuareg"          "ocaml")
          ("haskell"         "〉λ꞊")
          ("erlang"          "erl")
          ("sh"              "#!")
          ("menu"            "")
          ("mode"            "")
          ("package"         "↓")
          ("paradox"         "↓")
          ("nov"             "epub")
          ("docview"         "pdf")
          ("eshell"          "ⅇ/>_")
          ("term"            "†/>_")
          ("text"            "txt")
          ("org"             "ø")
          ("wdired"          "↯/dir"))))

;; (defmacro diminish-single (file mode &optional replacement)
;;   "Diminish a single MODE with optional REPLACEMENT name"
;;   `(with-eval-after-load ,(symbol-name file)
;;      (diminish (quote ,mode) ,replacement)))

;; (use-package diminish
;;   :init
;;   (progn
;;     (diminish-single company company-mode)
;;     (diminish-single flycheck flycheck-mode)
;;     (diminish-single geiser geiser-autodoc-mode)
;;     (diminish-single helm-mode helm-mode)
;;     (diminish-single slime slime-autodoc-mode)
;;     (diminish-single yasnippet yas-minor-mode)
;;     (diminish-single eldoc eldoc-mode)
;;     (diminish-single undo-tree undo-tree-mode)
;;     (diminish-single view view-mode)
;;     (diminish-single which-key which-key-mode)
;;     (diminish-single whitespace whitespace-mode)
;;     (diminish-single clj-refactor clj-refactor-mode)
;;     (diminish-single simple visual-line-mode)

;;     (diminish-single abbrev abbrev-mode " α ")
;;     (diminish-single parinfer parinfer-mode " π ")
;;     (diminish-single slime slime-mode " Σ ")
;;     (diminish-single geiser geiser-mode " γ ")
;;     (diminish 'isearch-mode " ⅈ ")))

(use-package powerline
  :init
  (setq powerline-text-scale-factor 1.05))

(use-package spaceline)
(use-package spaceline-all-the-icons
  :after
  spaceline
  :defer t
  :config
  (with-no-warnings
    (progn
      (spaceline-all-the-icons--setup-package-updates)
      (spaceline-all-the-icons--setup-paradox)
      (spaceline-all-the-icons--setup-neotree)
      (spaceline-all-the-icons--setup-anzu)
      (spaceline-toggle-all-the-icons-bookmark-on)
      (setq spaceline-all-the-icons-separator-type               'cup
            spaceline-all-the-icons-primary-separator            ")"
            spaceline-all-the-icons-secondary-separator          ")"
            spaceline-all-the-icons-window-number-always-visible t)
      (spaceline-all-the-icons-theme)))
  :custom-face
  (spaceline-highlight-face
   ((t (:background "#2257a0" :foreground "#bbc2cf"))))
  (powerline-active1
   ((t (:background "#202328" :foreground "#73797e"))))
  (powerline-active2
   ((t (:background "#202328" :foreground "#5b6268"))))
  (mode-line
   ((t (:background "#1b2229" :foreground "#5b6268"))))
  (powerline-inactive1
   ((t (:background "#23272e" :foreground "#5b6268"))))
  (powerline-inactive2
   ((t (:background "#23272e" :foreground "#5b6268"))))
  (mode-line-inactive
   ((t (:background "#23272e" :foreground "#5b6268")))))

(defun after-init-functions ()
  "Modes and functions to run after `after-init-hook'"
  (load-theme        'doom-one t)
  (cyphejor-mode     t)
  (spaceline-all-the-icons-theme)
  (blink-cursor-mode nil)
  (set-face-attribute 'vertical-border nil :foreground "#23272e"))

(add-hook 'after-init-hook #'after-init-functions)

(provide 'init-gui)
;;; init-gui.el ends here
