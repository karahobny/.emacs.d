;;; -*- lexical-binding: t; -*-
;;; keybindings.el --- global emacs keybindings
;;; Commentary:
;;;            global keybindings not fit into any other category

;;; Code:

(use-package tmm
  :defer t
  :bind  ("C-c b" . tmm-menubar))

(use-package which-key
  :defer    t
  :commands (which-key-mode)
  :init     (with-no-warnings
              (progn
                (which-key-setup-side-window-right-bottom)
                (which-key-mode))))

;; => windows & buffers
;; ==> buffers and switching them
(global-set-key (kbd "C-<right>")   #'next-buffer)
(global-set-key (kbd "C-<left>")    #'previous-buffer)
(global-set-key [mode-line mouse-4] #'next-buffer)
(global-set-key [mode-line mouse-5] #'previous-buffer)

;; ==> windows and moving between them
(windmove-default-keybindings    'meta)

(use-package ace-window
  :commands (ace-window)
  :bind     ("M-o" . ace-window))

(global-set-key "\C-x2"
                (lambda ()
                  (interactive)
                  (split-window-vertically)
                  (other-window 1)))
(global-set-key "\C-x3"
                (lambda ()
                  (interactive)
                  (split-window-horizontally)
                  (other-window 1)))

;; => text movement and manipulation
(global-set-key (kbd "<home>") #'beginning-of-line)
(global-set-key (kbd "<end>")  #'end-of-line)
(global-set-key (kbd "C-c g")  #'goto-line)
(global-set-key (kbd "RET")    #'newline-and-indent)

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

(global-set-key "\C-w" #'kill-region-or-word)

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

;; ==> control-tab to insert an actual tab character
(global-set-key (kbd "C-<tab>") "\C-q\t")

;; ==> meta-; to comment/uncomment lines, regions etc.
(use-package comment-dwim-2
  :defer t
  :bind  ("M-;" . comment-dwim-2))

;; ==> multiple cursors ala sublime
(global-unset-key (kbd "M-<down-mouse-1>"))
(use-package multiple-cursors
  :defer t
  :bind ("M-<mouse-1>" . mc/add-cursor-on-click))

(provide 'keybindings)
;;; keybindings.el ends here
