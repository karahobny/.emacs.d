;;; pkg-config.el --- package management
;;; Commentary:
;;;            The usual settings for MELPA and Paradox to replace the
;;;            regular package.el for browsing through repository.

;;; Code:
;; => melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives
                 '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; => paradox
(require 'paradox)
(paradox-enable)

(provide 'pkg-config)
;;; pkg-config.el ends here
