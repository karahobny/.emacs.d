;;; -*- lexical-binding: t; -*-
;;; init-multimedia.el --- Media, music, documents etc.

;;; Commentary:
;;;            epub-viewing with nov.el. Music through mpd to mingus.el. Some
;;;            messaging-related configuration. Pretty much everything that could
;;;            be called something else than coding; or pertaining to emacs'
;;;            behavior or its user interface.

;;; Code:
;;;; ** DOCUMENTS **
(use-package nov
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :bind
  (:map nov-mode-map
        ("C-p" . nov-previous-document)
        ("C-n" . nov-next-document)
        ("p"   . nov-scroll-up)
        ("n"   . nov-scroll-down)))

(use-package markdown-mode
  :defer t
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-asymmetric-header t))

;;;; ** MUSIC **
(defadvice mingus (after organize ())
  "Refresh and goto current song after entering mingus."
  (mingus-refresh)
  (mingus-goto-current-song))
(ad-activate 'mingus)

(use-package mingus
  :defer t
  :config
  (require 'mingus-stays-home))

;;;; ** MESSAGING **
(use-package all-the-icons-gnus
  :defer t
  :config
      (all-the-icons-gnus-setup))

(use-package twittering-mode
  :ensure f
  :defer t
  :commands twit
  :config
  (setq twittering-icon-mode         nil
        twittering-use-icon-storage  nil
        ;; twittering-icon-storage-file (concat user-emacs-directory
        ;;                                      ".twittering-mode-icons.gz")
        twittering-timer-interval    (* 60 2)
        twittering-status-format
        "%FACE[font-lock-constant-face]{%S}, %RT{%FACE[shadow]{retweeted by} %FACE[font-lock-constant-face]{%S} }%FACE[font-lock-doc-face]{%@} %FACE[shadow]{%p%r}\n%T\n"
        twittering-connection-type-order
          '(wget curl urllib-http native urllib-https)))


(provide 'init-multimedia)
;;; init-multimedia.el ends here
