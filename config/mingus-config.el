;;; -*- lexical-binding: t -*-
;;; mingus-config.el --- mpd client
;;; Commentary:
;;;            mingus can get tricky with it's handling of line
;;;            length so I hope these are enough to keep it at bay.

;;; Code:
(defadvice mingus (after organize ())
  "Refresh and goto current song after entering mingus."
  (mingus-refresh)
  (mingus-goto-current-song))
(ad-activate 'mingus)

(use-package mingus
  :defer  t
  :init   (progn
            (add-hook 'mingus-playlist-hooks
                      (lambda ()
                        (setq truncate-lines t))))
  :config (require 'mingus-stays-home)
  :bind   ("C-c m" . mingus))

(provide 'mingus-config)
;;; mingus-config.el ends here
