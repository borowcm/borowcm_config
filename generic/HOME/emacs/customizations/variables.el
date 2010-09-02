(defvar running-xemacs211 (string-match "21.1 (patch 14) \"Cuyahoga Valley\" XEmacs Lucid" emacs-version))
(defvar running-emacs2071  (string-match "20.7.1" emacs-version))

(setq truncate-lines nil)  ;; allow for overlap of lines when
			   ;; vertically splitting buffers
(setq truncate-partial-width-windows nil)
(setq gc-cons-threshold 250000) ;; increase GC count; occurs less frequently
(setq next-line-add-newlines nil)          ;;; Down arrow won't add \n at end
(setq require-final-newline t)             ;;; Put \n at end of last line

;; For automatically calling 'make -k'
(setq compilation-read-command t)

;; don want to use the default .signature file
(setq mail-signature-file "/home/rradhakr/.sign_off")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; For Abbrev-mode
(setq abbrev-file-name "~/.abbrev_defs")
(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)

(setq inhibit-startup-message t) ;; don't want to see initial message

;; set Info Directories
(setq Info-default-directory-list
      (append  '("/home/rradhakr/emacs"
		 "/home/rradhakr/info"
		 "/local/packages/gnu/lib/xemacs-19.14/info"
                 "/common/LaTeX2E/info"
		 "/local/packages/gnu/info")))

(setq auto-save-interval 2000)
(setq line-number-mode t)        ;; display current line number


;;  set text-mode as the major mode instead of fundamental-mode and
;;  automatically call auto-fill-mode.
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook
       '(lambda () (auto-fill-mode 1)))

;; full directory/name of the current buffer file
(setq frame-title-format
  '("%S: " (buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(add-hook 'write-file-hooks 'time-stamp)

(setq minibuffer-max-depth nil)

(add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)

;;; These variable settings have to come first, before the lines below.
(setq font-lock-use-fonts t)
(setq font-lock-use-colors t)
(setq font-lock-use-maximal-decoration t)

;; display column number
(setq column-number-mode t)

(setq auto-mode-alist
      (append '(("\\.cc$" . c++-mode)
                ("\\.hh$" . c++-mode)
                ("\\.C$" . c++-mode)
                ("\\.h$" . c-mode)
                ("\\.c$"  . c-mode)
                ("\\.py$"  . python-mode)
                ("\\.tex$"  . tex-mode)
		("\\.cshrc". sh-mode)
		("\\.cshrc.local". sh-mode)
		("Makefile.$" . makefile-mode)
		("makefile.$" . makefile-mode))
                  auto-mode-alist))

;; Load stuff explicitly for GNU/FSF Emacs
(when (or running-emacs2071)
;;  (menu-bar-mode -1)
  (setq transient-mark-mode t)
  (global-font-lock-mode t)
  (setq font-lock-support-mode 'lazy-lock-mode)
  (custom-set-variables
   '(compilation-read-command nil t)
   '(shell-pushd-tohome t)
   '(shell-pushd-dunique t)
   '(shell-pushd-dextract t)
   '(mode-line-format (quote ("-" (line-number-mode "L%l---") (column-number-mode "C%c--") mode-line-modified mode-line-frame-identification mode-line-buffer-identification "   " default-directory "  " global-mode-string "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--" (which-func-mode ("" which-func-format "--")) (-3 . "%p") "-%-")))
   '(scroll-bar-mode (quote right)))
(custom-set-faces
 '(font-lock-comment-face ((((class color) (background dark)) (:italic t :foreground "lightgrey"))))
 '(font-lock-keyword-face ((((class color) (background dark)) (:bold t :italic t :foreground "white"))))
 '(font-lock-constant-face ((t (:bold t :italic t :foreground "cyan2"))))
 '(font-lock-type-face ((t (:bold t :foreground "green"))))
 '(italic ((t (:italic t :foreground "lightblue"))))
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "yellow3"))))
 '(font-lock-function-name-face ((((class color) (background dark)) (:italic t :foreground "orange"))))
 '(bold ((t (:bold t :foreground "orange"))))
 '(font-lock-builtin-face ((((class color) (background dark)) (:italic nil :foreground "lightgreen"))))
 '(makefile-space-face ((((class color)) (:background "pink")))))
)

;; Load stuff explicitly for XEmacs/Lucid Emacs
(when (or running-xemacs211)
  ;; uncomment this line if you want your xemacs color/faces settings saved in ~/.xemacs-options
  (setq options-save-faces t)
  (set-specifier menubar-visible-p nil)
  (set-specifier right-toolbar-visible-p nil)
  (custom-set-variables
   '(compilation-read-command nil)
   '(mode-line-format (quote ("-" (line-number-mode "L%l---") (column-number-mode "C%c--") mode-line-modified mode-line-frame-identification mode-line-buffer-identification "   " default-directory "  " global-mode-string "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--" (which-func-mode ("" which-func-format "--")) (-3 . "%p") "-%-")))
   '(scroll-bar-mode (quote right))) 
(custom-set-faces
 '(red ((t (:foreground "red1"))) t)
 '(display-time-time-balloon-face ((t (:foreground "bisque"))))
 '(modeline-buffer-id ((t (:foreground "bisque"))) t)
 '(modeline-mousable ((t (:foreground "tan"))) t)
 '(modeline-mousable-minor-mode ((t (:foreground "lightgreen"))) t)
 '(blue ((t (:foreground "lightblue"))) t))
 ;;'(font-lock-mode t nil (font-lock))  
  (define-key global-map '(shift button3) 'mouse-function-menu)
  (autoload 'resize-minibuffer-mode "rsz-minibuf" nil t)
  (resize-minibuffer-mode)
  (setq resize-minibuffer-window-exactly nil)
  (display-column-mode 1)
  (setq display-time-mail-file t)
  (set-specifier modeline-shadow-thickness 1)
(setq fume-max-items 25
      fume-fn-window-position 3
	fume-auto-position-popup t
	fume-display-in-modeline-p t
	fume-menubar-menu-location "File"
	fume-buffer-name "*Function List*"
	fume-no-prompt-on-valid-default nil)
(unless window-system
  (blink-cursor-mode 0)
    )
  (setq bell-volume 0)
)

(when (or  running-xemacs211)
(custom-set-variables
 '(compilation-read-command nil)
 '(mouse-avoidance-mode nil nil (avoid))
 '(lazy-shot-mode t nil (lazy-shot))
 '(default-toolbar-position (quote right))
 '(scroll-bar-mode (quote right))
 '(mode-line-format (quote ("-" (line-number-mode "L%l---") (column-number-mode "C%c--") mode-line-modified mode-line-frame-identification mode-line-buffer-identification "   " default-directory "  " global-mode-string "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--" (which-func-mode ("" which-func-format "--")) (-3 . "%p") "-%-")) t)
 '(font-lock-mode t nil (font-lock)))
(custom-set-faces
 '(default ((t (:size "14pt" :family "Lucida Sans"))) t)
 '(red ((t (:foreground "red1"))) t)
 '(font-lock-reference-face ((((class color) (background dark)) (:foreground "brown2" :bold t :italic t))))
 '(display-time-time-balloon-face ((t (:foreground "bisque"))))
 '(modeline-buffer-id ((t (:foreground "bisque"))) t)
 '(modeline-mousable ((t (:foreground "tan"))) t)
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "tan" :bold t))))
 '(modeline-mousable-minor-mode ((t (:foreground "lightgreen"))) t)
 '(vhdl-font-lock-reserved-words-face ((((class color) (background dark)) (:foreground "Yellow" :bold t :italic t))))
 '(font-lock-keyword-face ((((class color) (background dark)) (:foreground "white" :bold t))))
 '(font-lock-comment-face ((((class color) (background dark)) (:italic t :foreground "lightgrey"))))
'(font-lock-type-face ((((class color) (background dark)) (:foreground "orange" :italic t))))
 '(blue ((t (:foreground "lightblue"))) t)
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "bisque" :bold t))))))
