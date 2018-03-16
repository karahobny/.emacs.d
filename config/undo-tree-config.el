;;; undo-tree-config.el --- undo-tree-mode related configuration
;;; Commentary:
;;;            Setting undo-tree-mode as global and keybindings for it.

;;; Code:
(use-package undo-tree
  :defer    t
  :diminish undo-tree-mode
  :config   (global-undo-tree-mode)
  :bind     (("C-z"   . undo-tree-undo)
             ("C-r"   . undo-tree-redo)
             ("C-Z"   . undo-tree-redo)
             ("C-c v" . undo-tree-visualize)))

(provide 'undo-tree-config)
;;; undo-tree-config.el ends here
