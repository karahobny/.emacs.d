;;; -*- lexical-binding: t; -*-
;;; init-modeline.el --- Modeline configuration bloated beyond belief

;;; Commentary:
;;;            Probably really, really, REALLY inefficient way to go about
;;;            this, but atleast it Works For Me (tm). Need to streamline
;;;            those unneccesary `require's away, but this feels so delicate
;;;            at the moment.

;;; Code:
(require 'all-the-icons)
;; FIXME: why the hell does this only work with use-package
;;        required here separately??
(require 'use-package)

(defvar mode-line-left-side-items nil
  "Items to display on the left-side of modeline.")
(defvar mode-line-right-side-items nil
  "Items to display on the right-side of modeline.")

(use-package spaceline
  :demand t
;;;; ** SPACELINE COLORSCHEME **
  :custom-face
  (spaceline-highlight-face
   ((t (:background "#2257a0" :foreground "#bbc2cf"))))
  (powerline-active1
   ((t (:background "#2257a0" :foreground "#bbc2cf"))))
  (powerline-active2
   ((t (:background "#202328" :foreground "#5b6268"))))
  (mode-line
   ((t (:background "#1b2229" :foreground "#5b6268"))))
  (powerline-inactive1
   ((t (:background "#1b2229" :foreground "#3f444a"))))
  (powerline-inactive2
   ((t (:background "#1b2229" :foreground "#3f444a"))))
  (mode-line-inactive
   ((t (:background "#282c34" :foreground "#5b6268"))))
  :config
  (progn
    (require 'spaceline-config)
    (spaceline-helm-mode)
    (spaceline-toggle-battery-on)
    (spaceline-toggle-hud-off)
    (spaceline-emacs-theme)
    (require 'powerline)
    (setq powerline-height 16
          powerline-default-separator 'wave)
    (require 'fancy-battery)
    (setq fancy-battery-show-percentage t)
    (fancy-battery-mode))
  (with-no-warnings
    (progn
      (setq spaceline-minor-modes-separator  " "
            spaceline-flycheck-bullet        "❖ %s"
            spaceline-byte-compile           t
            spaceline-separator-dir-left     '(left  . right)
            spaceline-separator-dir-right    '(right . left))))

;;;; ** SPACELINE SEGMENTS **
;; mostly scrapped from spaceline-all-the-icons, but with the benefit
;; of having more control over what's happening.

  (spaceline-define-segment ati-modified
    "An `all-the-icons' modified segment."
    (let* ((config-alist
            '(("*" all-the-icons-faicon-family all-the-icons-faicon
               "chain-broken" :height 1.2 :v-adjust -0.0)
              ("-" all-the-icons-faicon-family all-the-icons-faicon
               "link" :height 1.2 :v-adjust -0.0)
              ("%" all-the-icons-octicon-family all-the-icons-octicon
               "lock" :height 1.2 :v-adjust 0.1)))
           (result (cdr (assoc (format-mode-line "%*") config-alist))))
      (propertize (format "%s" (apply (cadr result) (cddr result)))
                  'face `(:family ,(funcall (car result)) :inherit)))
    :tight t)

  (spaceline-define-segment ati-mode-icon
    "An `all-the-icons' segment for the current buffer mode."
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon) ;; This implies it's the major mode
        (propertize icon
                    'help-echo (format "`%s`" major-mode)
                    'display '(raise 0.0)
                    'face `(:height 1.0
                            :family ,(all-the-icons-icon-family-for-buffer)
                            :inherit)))))

  (spaceline-define-segment ati-position
    "An `all-the-icons' segment for the Row and Column of the current point."
    (propertize (format-mode-line "☰ %l:%c ")
                'face `(:height 1.0 :inherit)
                'display '(raise 0.0)))

  (spaceline-define-segment ati-region-info
    "An `all-the-icons' segment for the currently marked region."
    (when mark-active
      (let ((words (count-lines (region-beginning) (region-end)))
            (chars (count-words (region-end) (region-beginning))))
        (concat
         (propertize (format "%s " (all-the-icons-octicon "pencil") words chars)
                     'face `(:family ,(all-the-icons-octicon-family) :inherit)
                     'display '(raise 0.0))
         (propertize (format "(%s, %s)" words chars)
                     'face `(:height 0.9 :inherit))))))

  (spaceline-define-segment ati-time
    "An `all-the-icons' segment for current time."
    (let* ((hour (string-to-number (format-time-string "%I")))
           (icon (all-the-icons-wicon (format "time-%s" hour) :v-adjust 0.5)))
      (concat
       (propertize (format "%s " icon)
                   'face `(:height 1.1
                           :family ,(all-the-icons-wicon-family)
                           :inherit)
                   'display '(raise 0.0))
       (propertize (format-time-string "%H:%M ")
                   'face `(:height 1.0 :inherit)
                   'display '(raise 0.0))))
   :tight t)

  (spaceline-define-segment
    ati-battery "An `all-the-icons' segment  with icon only provided (for now)."
    (propertize (format "%s" (all-the-icons-faicon "battery-full"))
                'face `(:height 1.0)
                'display '(raise 0.0)))

;;;; ** INITIALIZING THE MODELINE **
;; REFACTOR: `spaceline-install' is apparently the wrong way to go about this.
;;           need to switch to using `spaceline-compile' as soon as I dare.


  (spaceline-install
    'main
    ;; left side items
    '((ati-modified)
      (buffer-id)
      ((ati-mode-icon) (major-mode) (process :when active))
      (minor-modes))
    ;; right side items
    '((ati-region-info)
      (version-control :when active)
      ((flycheck-error flycheck-warning flycheck-info) :when active)
      (ati-position)
      ((ati-battery battery) :when active)
      ((ati-time " ") :when active))))

(provide 'init-modeline)
;;; init-modeline.el ends here
