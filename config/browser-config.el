;;; browser-config.el --- setting generic browser
;;; Commentary:
;;;            Overriding the default browse-url-generic-program
;;;            since OpenBSD names Chromium as ``chrome'', not ``chrome-bin''
;;;            or ``chromium-bin''.  Here is also room for development if I
;;;            ever need to configure EMACS' internal browsers.

;;; Code:
(require 'browse-url)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chrome")

(provide 'browser-config)
;;; browser-config.el ends here
