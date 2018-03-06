;;; undo-tree-config.el --- undo-tree-mode related configuration
;;; Commentary:
;;;            Setting undo-tree-mode as global and keybindings for it.

;;; Code:
(require 'undo-tree)
(global-undo-tree-mode)

;; => keybindings
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-r") 'undo-tree-redo)

(provide 'undo-tree-config)
;;; undo-tree-config.el ends here
