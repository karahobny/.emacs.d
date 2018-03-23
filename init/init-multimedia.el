;;; -*- lexical-binding: t; -*-
;;; init-multimedia.el --- Media, music, documents etc.

;;; Commentary:
;;;            How should I go on about dividing and
;;;            conjoining my initial initialization-files.

;;; Code:
;;;; DOCUMENTS
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


;;;; MUSIC
(defadvice mingus (after organize ())
  "Refresh and goto current song after entering mingus."
  (mingus-refresh)
  (mingus-goto-current-song))
(ad-activate 'mingus)

(use-package mingus
  :defer t
  :config
  (require 'mingus-stays-home))

;;;; EMAIL
(use-package all-the-icons-gnus
  :defer t
  :config
      (all-the-icons-gnus-setup))

(provide 'init-multimedia)
;;; init-multimedia.el ends here
