;;; keybindings.el --- global emacs keybindings
;;; Commentary:
;;;            global keybindings not fit into any other category

;;; Code:
(global-set-key (kbd "C-x C-m") #'execute-extended-command)
(global-set-key (kbd "C-c C-m") #'execute-extended-command)
(global-set-key (kbd "C-c b")   #'tmm-menubar)

;; => whichkey.el settings
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-right-bottom)

;; => windows & buffers
;; ==> buffers and switching them
(global-set-key [mode-line mouse-4] #'next-buffer)
(global-set-key [mode-line mouse-5] #'previous-buffer)

;; ==> windows and moving between them
(windmove-default-keybindings    'meta)
(global-set-key (kbd "M-<tab>") #'other-window)
(global-set-key (kbd "M-o")     #'ace-window)

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
(global-set-key (kbd "C-a")    #'back-to-indentation)
(global-set-key (kbd "C-S-a")  #'beginning-of-line)

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
(global-set-key (kbd "M-;") #'comment-dwim-2)

;; ==> multiple cursors ala sublime
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key   (kbd "M-<mouse-1>") #'mc/add-cursor-on-click)

(provide 'keybindings)
;;; keybindings.el ends here
