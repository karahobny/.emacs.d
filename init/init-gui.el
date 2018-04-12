;;; -*- lexical-binding: t; -*-
;;; init-gui.el --- prettifying Emacs piece-by-piece

;;; Commentary:
;;;            General user-interface stuff that felt out of place
;;;            anywhere else. (Modeline used to cover almost half of this
;;;            so I decided it was time to break out it an init-file of
;;;            it's own.

;;; Code:
;;;; *** GENERAL UX ***
(setq menu-bar-mode      nil
      tool-bar-mode      nil
      use-dialog-box     nil
      blink-cursor-mode  nil
      scroll-bar-mode    nil
      fringe-mode        nil
      confirm-kill-emacs 'y-or-n-p)

(use-package tmm
  :ensure f
  :bind   ("C-c b" . tmm-menubar))

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

(use-package which-key
  :defer    50
  :diminish which-key-mode
  :init     (progn
              (which-key-setup-side-window-right-bottom)
              (which-key-mode)))


;;;; *** BUFFERS ***
(defalias 'list-buffers #'ibuffer)
(global-set-key (kbd "C-<right>") #'next-buffer)
(global-set-key (kbd "C-<left>")  #'previous-buffer)

(use-package nlinum
  :config (progn
            (setq nlinum-format "%4d ")
            (use-package nlinum-relative
              :bind        (("M-n"     . nlinum-relative-toggle)
                            ("C-c r"   . nlinum-relative-toggle)
                            ("C-c C-r" . nlinum-relative-toggle))
              :custom-face (nlinum-relative-current-face
                            ((t :inherit    linum     :weight     bold
                                :foreground "#bbc2cf" :background "#23272e")))))
  :hook   (prog-mode . nlinum-mode))

(use-package paren
  :ensure f
  :config (progn
            (setq show-paren-delay                   0
                  show-paren-highlight-openparen     t
                  show-paren-when-point-inside-paren t
                  show-paren-when-point-in-periphery t))
  :hook   (after-init . show-paren-mode))

(use-package indent-guide
  :diminish    indent-guide-mode
  :config      (progn
                 (setq indent-guide-char "¦")
                 (indent-guide-global-mode))
  :hook        ((prog-mode text-mode) . indent-guide-mode)
  :custom-face (indent-guide-face
                ((t :foreground "dimgray" :background "default"))))

;; prog-mode initialized just for ``prettify-symbols-mode''
(use-package prog-mode
  :ensure   f
  :defer    t
  :commands (prettify-symbols-mode global-prettify-symbols-mode)
  :config   (progn
              (setq prettify-symbols-alist
                    '(("lambda" . 955)))
              (global-prettify-symbols-mode +1))
  :hook     (prog-mode . prettify-symbols-mode))

(use-package simple
  :ensure    f
  :diminish  visual-line-mode
  :config    (progn
               (setq delete-trailing-lines t))
  :init      (global-visual-line-mode)
  :bind      ("C-c g" . goto-line)
  :hook      (before-save . delete-trailing-whitespace))

(use-package browse-url
  :ensure f
  :defer  t
  :init   (setq browse-url-browser-function 'browse-url-generic
                browse-url-generic-program  "chrome"))


;;;; *** WINDOWS / LAYOUTS ***
(use-package winner
  :defer  50
  :config (winner-mode 1)
  :bind   (("M-N" . winner-redo)
           ("M-P" . winner-undo)))

(use-package windmove
  :bind (("M-<up>"    . windmove-up)
         ("M-<left>"  . windmove-left)
         ("M-<down>"  . windmove-down)
         ("M-<right>" . windmove-right)))

(use-package ace-window
  :bind (("M-o"   . ace-window)
         ("C-c o" . ace-window)))

;;    ((super + [left/up/down/...) bound in ~/.exwm-x to not accientally override
;;     or more likely just plain overlap other possible window manager's
;;     and their probable super-centric bindings. (tiling wms, cwm, etc.))

(defvar win-resize-step nil
  "The minimum amount of delta columns to shrink and/or enlarge a window.")

(defvar dynamic-window-resizing-step nil
  "Whether to use frame size as an indicator using dimensions to multiply
WIN-RESIZE-STEP. For example, this  allows #'enlarge-window-horizontally to be
resized more per step than #'enlarge-window (vertical window resizing)  when the
FRAME-WIDTH is larger than FRAME-HEIGHT.")

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

(global-set-key (kbd "C-x 2") #'my-split-win-vert)
(global-set-key (kbd "C-x 3") #'my-split-win-horiz)

(defun my-shrink-win-horiz ()
  "Shrink window horizontally by WIN-RESIZE-STEP-H"
  (interactive)
  (shrink-window-horizontally (win-resize-step-h)))
(defun my-enlarge-win-horiz ()
  "Enlarge window horizontallyy by WIN-RESIZE-STEP-H"
  (interactive)
  (enlarge-window-horizontally (win-resize-step-h)))
(defun my-shrink-win-vert ()
  "Shrink window vertically by WIN-RESIZE-STEP-V"
  (interactive)
  (shrink-window (win-resize-step-v)))
(defun my-enlarge-win-vert ()
  "Enlarge window vertically by WIN-RESIZE-STEP-V"
  (interactive)
  (enlarge-window (win-resize-step-v)))

(global-set-key (kbd "M-S-<up>")    #'my-shrink-win-vert)
(global-set-key (kbd "M-S-<down>")  #'my-enlarge-win-vert)
(global-set-key (kbd "M-S-<left>")  #'my-shrink-win-horiz)
(global-set-key (kbd "M-S-<right>") #'my-enlarge-win-horiz)


;;;; *** HELM ***
(use-package helm
  :defer    t
  :diminish helm-mode
  :config   (progn
              (use-package helm-config  :ensure f)
              (setq helm-split-window-inside-p        t
                    helm-move-to-line-cycle-in-source t
                    helm-echo-input-in-header-line    t))
  :init     (helm-mode)
  :bind     (("M-x"     . helm-M-x)
             ("C-x C-m" . helm-M-x)
             ("C-c C-m" . helm-M-x)
             ("C-x C-f" . helm-find-files)
             ("C-c M-i" . helm-imenu)))

;; changed default HELM-GOOGLE-ACTIONS to use eww by default.
(use-package helm-google
  :defer  t
  :config (progn
            (setq helm-google-idle-delay 1
                  helm-google-actions
                  '(("Browse URL with EWW"
                     . (lambda (candidate) (eww-browse-url candidate)))
                    ("Browse URL with default program" . browse-url)
                    ("Copy URL to clipboard"
                     . (lambda (candidate) (kill-new candidate))))))
  :bind   ("C-h C--" . helm-google))


;;;; *** USER INPUT ***
(global-set-key (kbd "RET")    #'newline-and-indent)
(global-set-key (kbd "<home>") #'beginning-of-line)
(global-set-key (kbd "<end>")  #'end-of-line)

;; control-tab to insert an actual tab-char
(global-set-key (kbd "C-<tab>") "\C-q\t")

;; yes-or-no-p -> y-or-no-p with return-key aliased to confirmation
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m]  'act)

(use-package undo-tree
  :defer     t
  :diminish  undo-tree-mode
  :functions global-undo-tree-mode
  :config    (progn
               (global-undo-tree-mode))
  :bind      (("C-z"   . undo-tree-undo)
              ("C-r"   . undo-tree-redo)
              ("C-Z"   . undo-tree-redo)
              ("C-c v" . undo-tree-visualize)))

(use-package isearch
  :ensure   f
  :diminish (isearch-mode . "ⅈ"))

(use-package centered-cursor-mode
  :defer    t
  :diminish (centered-cursor-mode . "-+-")
  :bind     ("C-c -"   . centered-cursor-mode))

(use-package comment-dwim-2
  :defer t
  :bind  ("M-;" . comment-dwim-2))

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(global-set-key (kbd "C-a") #'smart-line-beginning)

(defun kill-region-or-word ()
  "Call `kill-region' or `backward-kill-word'.
Depending on whether or not a region is selected."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (kill-region (point) (mark))
    (backward-kill-word 1)))

(global-set-key (kbd "C-w") #'kill-region-or-word)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") #'eval-and-replace)


;;;; *** HIGHLIGHTS ***
(defvar column-char-limit 82
  "Highlight characters going over the specified amount of columns/characters.")

(use-package whitespace
  :ensure   f
  :defer    t
  :diminish whitespace-mode
  :config   (progn
              (setq whitespace-line-column column-char-limit
                    whitespace-style       '(face lines-tail)))
  ;; (dolist (mode-str char-limited-modes)
  ;;   (add-hook mode-str #'whitespace-mode)))
  :hook     ((prog-mode
              emacs-lisp-mode
              lisp-mode
              common-lisp-mode
              scheme-mode
              clojure-mode
              haskell-mode
              tuareg-mode)
             . whitespace-mode))


(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(defvar font-annotated-modes
  '(prog-mode-hook
    emacs-lisp-mode-hook
    lisp-mode-hook
    common-lisp-mode-hook
    scheme-mode-hook
    clojure-mode-hook
    haskell-mode-hook
    tuareg-mode-hook)
  "List of programming hooks to apply #'font-lock-comment-annotations to.")

(dolist (mode-str font-annotated-modes)
  (add-hook mode-str #'font-lock-comment-annotations))


;;;; *** USER THEMES ***
(setq custom-safe-themes t)
(use-package doom-themes
  :ensure f
  :demand t
  :config (progn
            (doom-themes-neotree-config)
            (doom-themes-org-config)
            (set-face-attribute 'vertical-border nil :foreground "#23272e"))
  :init   (load-theme 'doom-one t))


;;;; *** DIMINISHING MODES ***
(use-package cyphejor
  :demand t
  :hook   (after-init . cyphejor-mode)
  :config (progn
            (setq cyphejor-rules
                  '(:downcase
                    ("exwm"            "ⅇ𝕩")
                    ("apropos"         "apropos")
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
                    ("racket"          "(λ)")
                    ("repl"            "")
                    ("sml"             "ml")
                    ("tuareg"          "cαml")
                    ("haskell"         "〉λ꞊")
                    ("sh"              "#!")
                    ("menu"            "")
                    ("mode"            "")
                    ("package"         "↓")
                    ("paradox"         "↓")
                    ("nov"             "（´ω ` * ）")
                    ("docview"         "（　´_ゝ`）")
                    ("eshell"          "ⅇ/>_")
                    ("term"            "†/>_")
                    ("text"            "txt")
                    ("org"             "ø")
                    ("wdired"          "↯/dir")))))

(provide 'init-gui)
;;; init-gui.el ends here
