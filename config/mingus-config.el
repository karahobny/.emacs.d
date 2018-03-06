;;; mingus-config.el --- mpd client
;;; Commentary:
;;;            mingus can get tricky with it's handling of line
;;;            length so I hope these are enough to keep it at bay.

;;; Code:
(require 'mingus)
(require 'mingus-stays-home)

(defadvice mingus (after organize ())
  "Refresh and goto current song after entering mingus."
  (mingus-refresh)
  (mingus-goto-current-song))
(ad-activate 'mingus)

;; => truncating lines if mingus decides to be annoying
(add-hook 'mingus-playlist-hooks
          (lambda ()
            (setq truncate-lines t)
            (local-set-key (kbd "C-c t") #'toggle-truncate-lines)))

;; => keybindings
(global-set-key (kbd "C-c m") #'mingus)

(provide 'mingus-config)
;;; mingus-config.el ends here
