(require 'autorevert)

(defun auto_update_design( arg )
  " Automatically update design file"
  (interactive)
  (find-file arg)
  (auto-revert-mode t)
  (setq major-mode 'auto-revert-mode))
