;;; -*- lexical-binding: t; -*-
;;; init-gui.el --- prettifying Emacs piece-by-piece

;;; Commentary:
;;;            General user-interface stuff that felt out of place
;;;            anywhere else. (Modeline used to cover almost half of this
;;;            so I decided it was time to break out it an init-file of
;;;            it's own.

;;; Code:
;;;; ** BUFFERS **
(global-visual-line-mode 1)
(show-paren-mode         1)

(defalias 'list-buffers #'ibuffer)

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
  :demand t
  :init
  (setq nlinum-format "%4d ")
  :config
  (add-hook 'prog-mode-hook #'nlinum-mode))

(use-package nlinum-relative
  :after nlinum
  :bind
  (("M-n"     . nlinum-relative-toggle)
   ("C-c r"   . nlinum-relative-toggle)
   ("C-c C-r" . nlinum-relative-toggle))
  :custom-face
  (nlinum-relative-current-face
   ((t :inherit linum :foreground "#bbc2cf" :background "#23272e" :weight bold))))

;; prog-mode initialized just for ``prettify-symbols-mode''
(use-package prog-mode
  :ensure f
  :defer t
  :commands
  (prettify-symbols-mode global-prettify-symbols-mode)
  :init
  (setq prettify-symbols-alist '(("lambda" . 955)))
  (add-hook 'prog-mode-hook #'prettify-symbols-mode)
  :config
  (global-prettify-symbols-mode))

(use-package browse-url
  :ensure f
  :defer t
  :init
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program  "chrome"))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;;; ** WINDOWS / LAYOUTS
(winner-mode t)
(windmove-default-keybindings 'meta)

(use-package ace-window
  :defer t
  :bind
  (("M-o"   . ace-window)
   ("C-c o" . ace-window)))
;; => window resizing with vim-like keybindings.

;;    ((super + [left/up/down/...) bound in ~/.exwm-x to not accientally override
;;     or more likely just plain overlap other possible window manager's
;;     and their probable super-centric bindings. (tiling wms, cwm, etc.))

(defvar win-resize-step nil
  "The minimum amount of delta columns to shrink and/or enlarge a window.")

(defvar dynamic-window-resizing-step nil
  "Whether to use frame size as an indicator using dimensions to multiply
WIN-RESIZE-STEP. For example, this  allows `enlarge-window-horizontally' to be
resized more than `enlarge-window' (vertical window resizing)  when the
FRAME-WIDTH is larger than FRAME-HEIGHT.")

(defvar dynamic-window-resizing-positional nil
  "Resize or shrink windows dynamically depending on the window's location.")


(setq win-resize-step              3
      dynamic-window-resizing-step t)

;; stale hack i put together to get the numbers for resizing differently when
;; frame is wider or taller

(defun win-resize-step-h ()
  "The amount of delta columns to shrink and/or enlarge a window horizontally"
  (if (bound-and-true-p dynamic-window-resizing-step)
      (if (> (frame-width) (frame-height))
          (* (/ (frame-width) (frame-height)) win-resize-step)
        win-resize-step)
    win-resize-step))

(defun win-resize-step-v ()
  "The amount of delta columns to shrink and/or enlarge a window vertically."
  (if (bound-and-true-p dynamic-window-resizing-step)
      (if (> (frame-height) (frame-width))
          (* (/ (frame-height) (frame-width)) win-resize-step)
        win-resize-step)
    win-resize-step))

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

;; dynamic window resizing depending on position
;; source: https://www.emacswiki.org/emacs/WindowResize

(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the middle"
  (let* ((win-edges (window-edges))
         (this-window-y-min (nth 1 win-edges))
         (this-window-y-max (nth 3 win-edges))
         (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min)               "t")
     ((eq (- fr-height 1) this-window-y-max) "b")
     (t                                      "m"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the middle"
  (let* ((win-edges (window-edges))
         (this-window-x-min (nth 0 win-edges))
         (this-window-x-max (nth 2 win-edges))
         (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min)              "l")
     ((eq (+ fr-width 4) this-window-x-max) "r")
     (t                                     "m"))))

(defun my-shrink-win-horiz ()
  "Shrink window horizontally by WIN-RESIZE-STEP-H"
  (interactive)
  (if (bound-and-true-p dynamic-window-resizing-positional)
      (cond
        ((equal "t" (win-resize-top-or-bot))
         (shrink-window-horizontally (win-resize-step-h)))
        ((equal "b" (win-resize-top-or-bot))
         (enlarge-window-horizontally (win-resize-step-h)))
        ((equal "m" (win-resize-top-or-bot))
         (shrink-window-horizontally (win-resize-step-h)))
        (t (message "nil")))
    (shrink-window-horizontally (win-resize-step-h))))

(defun my-enlarge-win-horiz ()
  "Enlarge window horizontallyy by WIN-RESIZE-STEP-H"
  (interactive)
  (if (bound-and-true-p dynamic-window-resizing-positional)
    (cond
      ((equal "t" (win-resize-top-or-bot))
       (enlarge-window-horizontally (win-resize-step-h)))
      ((equal "b" (win-resize-top-or-bot))
       (shrink-window-horizontally (win-resize-step-h)))
      ((equal "m" (win-resize-top-or-bot))
       (enlarge-window-horizontally (win-resize-step-h)))
      (t (message "nil")))
    (enlarge-window-horizontally (win-resize-step-h))))

(defun my-shrink-win-vert ()
  "Shrink window vertically by WIN-RESIZE-STEP-V"
  (interactive)
  (if (bound-and-true-p dynamic-window-resizing-positional)
    (cond
      ((equal "l" (win-resize-left-or-right))
       (shrink-window (win-resize-step-v)))
      ((equal "r" (win-resize-left-or-right))
       (enlarge-window (win-resize-step-v)))
      ((equal "m" (win-resize-left-or-right))
       (shrink-window (win-resize-step-v)))
      (t (message "nil")))
   (shrink-window (win-resize-step-v))))

(defun my-enlarge-win-vert ()
  "Enlarge window vertically by WIN-RESIZE-STEP-V"
  (interactive)
  (if (bound-and-true-p dynamic-window-resizing-positional)
    (cond
      ((equal "l" (win-resize-left-or-right))
       (enlarge-window (win-resize-step-v)))
      ((equal "r" (win-resize-left-or-right))
       (shrink-window (win-resize-step-v)))
      ((equal "m" (win-resize-left-or-right))
       (enlarge-window (win-resize-step-v)))
      (t (message "nil")))
    (enlarge-window (win-resize-step-v))))

(global-set-key (kbd "C-x 2")       #'my-split-win-vert)
(global-set-key (kbd "C-x 3")       #'my-split-win-horiz)

(global-set-key (kbd "M-S-<up>")    #'my-shrink-win-vert)
(global-set-key (kbd "M-S-<down>")  #'my-enlarge-win-vert)
(global-set-key (kbd "M-S-<left>")  #'my-shrink-win-horiz)
(global-set-key (kbd "M-S-<right>") #'my-enlarge-win-horiz)

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

;; changed default ``helm-google-actions'' to prefer eww first.
(use-package helm-google
  :defer t
  :config
  (setq helm-google-idle-delay 1
        helm-google-actions
        '(("Browse URL with EWW"             . (lambda (candidate)
                                                 (eww-browse-url candidate)))
          ("Browse URL with default program" . browse-url)
          ("Copy URL to clipboard"           . (lambda (candidate)
                                                 (kill-new candidate)))))
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
  :ensure f
  :demand t
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

;;;; ** USER THEMES **
(setq custom-safe-themes t)
;; (use-package all-the-icons)

;;;; ** DIMINISHING MODES **
;; mostly meaningless due to spaceline, but some of these are still needed
;; and most are kept just in case. you know: if i ever get to crap out a fully
;; artisanal powerline-ish modeline of my own from scratch. and these
;; abbreviations are still found on ibuffer for example.
;; same stands for the commented out diminishes (most likely i would delegate
;; it to use-package's parameters).
(use-package cyphejor
  :defer t
  :config
  (setq cyphejor-rules
        '(:downcase
          ("exwm"            "ⅇ𝕩")
          ("apropos"         "α*")
          ("bookmark"        "→")
          ("messages"        "msg")
          ("buffer"          "")
          ("fundamental"     "")
          ("special"         "")
          ("hmm"             "")
          ("help"            "?")
          ("ibuffer"         "β")
          ("compilation"     "!")
          ("compiling"       "...")
          ("custom"          "custom")
          ("diff"            "diff")
          ("dired"           "dir")
          ("debug"           "!")
          ("helm"            "")
          ("esup"            "GOTTA GO FAST")
          ("emacs"           "ⅇ")
          ("inferior"        "𝕚" :prefix)
          ("interaction"     "𝕚" :prefix)
          ("interactive"     "𝕚" :prefix)
          ("emacs-lisp"      "ⅇλ")
          ("suggest"         "Σⅇλ")
          ;; ("lisp"            "λ")
          ;; ("scheme"          "λ")
          ("lisp"            "λ")
          ("scheme"          "Σ")
          ("clojure"         "𝕔λⅉ")
          ;; ("clojure"         "cλj")
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
          ("nov"             "（´ω ` * ）")
          ("docview"         "（　´_ゝ`）")
          ("mingus"          "μpd")
          ("eshell"          "ⅇ/>_")
          ("term"            "†/>_")
          ("text"            "txt")
          ("org"             "ø")
          ("wdired"          "↯/dir"))))

(defmacro diminish-single (file mode &optional replacement)
  "Diminish a single MODE with optional REPLACEMENT name"
  `(with-eval-after-load ,(symbol-name file)
     (diminish (quote ,mode) ,replacement)))

(use-package diminish
  :demand t
  :config
  (progn
    (diminish-single company company-mode)
    (diminish-single flycheck flycheck-mode)
    (diminish-single geiser geiser-autodoc-mode)
    (diminish-single helm-mode helm-mode)
    (diminish-single slime slime-autodoc-mode)
    (diminish-single yasnippet yas-minor-mode)
    (diminish-single undo-tree undo-tree-mode)
    (diminish-single view view-mode)
    (diminish-single which-key which-key-mode)
    (diminish-single whitespace whitespace-mode)
    (diminish-single clj-refactor clj-refactor-mode)
    (diminish-single simple visual-line-mode)

    (diminish-single eldoc eldoc-mode "ⅇδ")
    (diminish-single abbrev abbrev-mode "α")
    (diminish-single parinfer parinfer-mode "π")
    (diminish-single slime slime-mode "Σ")
    (diminish-single geiser geiser-mode "γ")
    (diminish 'isearch-mode "ⅈ")
    (diminish 'geiser-mode  "γ")))

;;;; ** AFTER-INIT **
(defun after-init-functions ()
  "Modes and functions to run after `after-init-hook'"
  (load-theme            'doom-one t)
  (cyphejor-mode         1)
  (blink-cursor-mode     0)
  (set-face-attribute 'vertical-border nil :foreground "#23272e"))

(add-hook 'after-init-hook #'after-init-functions)

(provide 'init-gui)
;;; init-gui.el ends here
