;;; -*- lexical-binding: t; -*-
;;; init-multimedia.el --- Media, music, documents etc.

;;; Commentary:
;;;            epub-viewing with nov.el. Music through mpd to mingus.el. Some
;;;            messaging-related configuration. Pretty much everything that could
;;;            be called procrastinating.

;;; Code:
;;;; *** DOCUMENTS ***
(use-package nov
  :defer t
  :mode  ("\\.epub\\'" . nov-mode)
  :bind  (:map nov-mode-map
               ("C-p" . nov-previous-document)
               ("C-n" . nov-next-document)
               ("p"   . nov-scroll-up)
               ("n"   . nov-scroll-down)))

(use-package markdown-mode
  :defer  t
  :mode   ("\\.\\(md\\|markdown\\)\\'" . markdown-mode)
  :config (setq markdown-asymmetric-header t))


;;;; *** MUSIC ***
(defadvice mingus (after organize ())
  "Refresh and goto current song after entering mingus."
  (mingus-refresh)
  (mingus-goto-current-song))
(ad-activate 'mingus)

(use-package mingus
  :defer  t
  :config (use-package mingus-stays-home :ensure f))

(use-package emms
  :defer   t
  :defines emms-source-file-default-directory
  :bind    (("C-c M p" . emms-previous)
            ("C-c M n" . emms-next)
            ("C-c M s" . emms-show)
            ("C-c M k" . emms-stop))
  :config  (progn
             (setq emms-source-file-default-directory "~/music/")
             (use-package emms-setup   :ensure f)
             (use-package emms-browser :ensure f))
  :init    (progn
             (emms-all)
             (emms-default-players)))


;;;; *** MESSAGING ***
(use-package all-the-icons-gnus
  :defer t
  :config (all-the-icons-gnus-setup))

(provide 'init-multimedia)
;;; init-multimedia.el ends here
