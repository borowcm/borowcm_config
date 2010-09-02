(unless window-system
  (setq key-translation-map (make-sparse-keymap))
  (define-key key-translation-map "\177" "\C-h")
  (define-key key-translation-map "\C-h" "\177")
  (defvar BACKSPACE "\177")
  (defvar DELETE    "\C-h")
  (global-set-key BACKSPACE 'backward-delete-char)
  (auto-fill-mode)
  )
