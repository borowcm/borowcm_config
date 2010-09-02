(setq mail-signature-file "/home/rradhakr/.sign_off")
(setq default-major-mode 'mail-mode)
(add-hook 'mail-mode-hook
       '(lambda () (auto-fill-mode 1)))
(menu-bar-mode -1)

(defun my-insert-signature() 
  (interactive)
  (progn 
    (insert-file-contents (expand-file-name "/home/rradhakr/.sign_off")))
  (save-buffer))
(define-key global-map "\C-ci" 'my-insert-signature)

(setq key-translation-map (make-sparse-keymap))
(define-key key-translation-map "\177" "\C-h")
(define-key key-translation-map "\C-h" "\177")
(defvar BACKSPACE "\177")
(defvar DELETE    "\C-h")
(global-set-key BACKSPACE 'backward-delete-char)
(define-key global-map "\C-cs" 'shell)
