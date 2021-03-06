x(setq load-path (append load-path
                        (list nil 
			      (expand-file-name "/home/rradhakr/emacs")
			      (expand-file-name "/home/ddelwork/rradhakr/auctex-9.9p")
			      )))

;; for emacsclient
;;(server-start)

(add-hook 'diary-hook 'appt-make-list)

;; using the Common Lisp (CL) package
(require 'cl)

;; Load DOC++ commands auto-insert functions
(load "/home/rradhakr/emacs/doc++.el")

;; allow for overlap of lines when vertically splitting buffers
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq pixel-vertical-clip-threshold 30)
(setq gc-cons-threshold 250000) ;; increase GC count; occurs less frequently
(setq abbrev-file-name "~/.abbrev_defs")
(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)

;; project.el in ~/emacs/
;; Automatically opens files belonging to current project
;; and opens an 'ssh' shell on papaya 
;;(load "project")

(defun line-to-top-of-window ()
  "Move the line point is on to top of window."
  (interactive) 
  (recenter 0))

;; CC-Mode 5.xx customization
(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open after)
				   (defun-block-intro after)
				   (defun-open after)
				   (class-open after)
				   (inline-open after)
				   (inline-close after)
				   (block-close)
				   (extern-lang-open after)))
    (c-hanging-colons-alist     . ((inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
				   (inline-open       . 0)
                                   (block-open        . 0)
                                   (brace-list-entry  . +)
                                   (statement         . 0)
				   (comment-intro     . 0)
				   (access-label      . -6)
				   ))
    )
  "My C Programming Style")

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "PERSONAL" my-c-style t)
  ;; offset customizations not in my-c-style
  (c-set-offset 'member-init-intro '+)
  ;; other customizations
  (setq c-basic-offset 3)

  (setq tab-width 2
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-state 1)
  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;; -----------------
;;; --- autoloads ---
;;; -----------------
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)

(autoload 'map-switch-to-associated-buffer "/home/rradhakr/emacs/switch-assoc.el"
  "Switch to an associated buffer" t)

(setq auto-mode-alist
      (append '(("\\.cc$" . c++-mode)
                ("\\.hh$" . c++-mode)
                ("\\.C$" . c++-mode)
                ("\\.h$" . c-mode)
                ("\\.c$"  . c-mode)
                ("\\.py$"  . python-mode)
                ("\\.tex$"  . tex-mode)
		("\\.cshrc". shell-mode)
		("\\.cshrc.local". shell-mode)
		("Makefile.$" . makefile-mode)
		("makefile.$" . makefile-mode))
                  auto-mode-alist))

;;(add-hook 'c-mode-common-hook 'abbrev-mode)

;; for python
(autoload 'python-mode "python-mode" nil t)                             

;; Load verilog mode only when needed
(autoload 'verilog-mode "verilog-mode" "Verilog Editing mode" t )
;; Any files that end in .v should be in verilog mode
(setq auto-mode-alist (append  '("\\.v\\'" . verilog-mode) auto-mode-alist))
;; Any files in verilog mode should have their keywords colorized
(add-hook 'verilog-mode-hook '(lambda () (font-lock-mode 1)))

;; for ANTLR 
(autoload 'antlr-mode "antlr-mode" nil t)                  
(push '("\\.g\\'" . antlr-mode) auto-mode-alist)                      
(add-hook 'speedbar-load-hook  ; would be too late in antlr-mode.el   
	  (lambda () (speedbar-add-supported-extension ".g")))        
(autoload 'antlr-set-tabs "antlr-mode")

;; Load CPerl mode
(setq auto-mode-alist
      (append '(("\\.[pP][Llm]$" . cperl-mode))  auto-mode-alist ))
(setq interpreter-mode-alist (append interpreter-mode-alist
 				        '(("miniperl" . cperl-mode))))
(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(setq cperl-hairy t)
(setq cperl-indent-level 2)
(setq cperl-electric-keywords t)

;; Miscellaneous Commands
;; set Info Directories
(setq Info-default-directory-list
      (append  '("/home/rradhakr/emacs"
		 "/home/rradhakr/info"
		 "/local/packages/gnu/lib/xemacs-19.14/info"
                 "/common/LaTeX2E/info"
		"/local/packages/gnu/info")))

;; Define backspace key for 'xemacs/emacs -nw'
(load "backspace.elc")

(setq diary-file "~/.diary")
(setq auto-save-interval 2000)
(setq compile-command "make")
(setq line-number-mode t)        ;; display current line number
(setq inhibit-startup-message t) ;; don't want to see initial message
(setq display-time-day-and-date t)
(display-time)

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

;; I always like to open ssh shell to my "compile machine" : papaya 
;; and using this hot-key function I can "switch" to it easily.
(defun my-switch-to-papaya() ;; mapped to 'C-c p'
  (interactive)
  (ssh "papaya"))

(when (or running-emacs2031 running-emacs2061 running-emacs2041)

  (require 'color-theme)
  (color-theme-ryerson)
  ;;  (color-theme-sitaramv-solaris)
)

(when (or running-emacs19 running-emacs2031 running-emacs2061 running-emacs2041)
  ;; automatically unzips, untars files  
  (menu-bar-mode -1)
  (require 'uncompress)
;;  (load "/home/rradhakr/emacs/ssh.elc")
;;  (require 'ssh)
  (global-font-lock-mode t)
  (setq display-time-mail-file t)
  (setq font-lock-support-mode 'lazy-lock-mode)
  (custom-set-variables
   '(mode-line-format (quote ("-" (line-number-mode "L%l---") (column-number-mode "C%c--") mode-line-modified mode-line-frame-identification mode-line-buffer-identification "   " default-directory "  " global-mode-string "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--" (which-func-mode ("" which-func-format "--")) (-3 . "%p") "-%-")))
   '(scroll-bar-mode (quote right)))
  (custom-set-faces)
)


;; Load stuff explicitly for XEmacs/Lucid Emacs
(when (or running-xemacs20 running-xemacs204 running-xemacs21R running-xemacs19)
  ;; uncomment this line if you want your xemacs color/faces settings saved in ~/.xemacs-options
  ;; (setq options-save-faces t)
  (set-specifier menubar-visible-p nil)
  (set-specifier right-toolbar-visible-p nil)
;;  (require 'func-menu)
;;  (add-hook 'find-file-hooks 'fume-add-menubar-entry)
;;  (define-key global-map "\C-cl" 'fume-list-functions)
;;  (define-key global-map "\C-cg" 'fume-prompt-function-goto)

  (custom-set-variables
   '(mode-line-format (quote ("-" (line-number-mode "L%l---") (column-number-mode "C%c--") mode-line-modified mode-line-frame-identification mode-line-buffer-identification "   " default-directory "  " global-mode-string "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--" (which-func-mode ("" which-func-format "--")) (-3 . "%p") "-%-")))
   '(scroll-bar-mode (quote right))) 
(custom-set-faces
 '(red ((t (:foreground "red1"))) t)
 '(display-time-time-balloon-face ((t (:foreground "bisque"))))
 '(modeline-buffer-id ((t (:foreground "bisque"))) t)
 '(modeline-mousable ((t (:foreground "tan"))) t)
 '(modeline-mousable-minor-mode ((t (:foreground "lightgreen"))) t)
 '(blue ((t (:foreground "lightblue"))) t))
 '(font-lock-mode t nil (font-lock))  
  (define-key global-map '(shift button3) 'mouse-function-menu)
  (require 'crypt)
  ;; automatically resize minibuffer
  (autoload 'resize-minibuffer-mode "rsz-minibuf" nil t)
  (resize-minibuffer-mode)
  (setq resize-minibuffer-window-exactly nil)
  (display-column-mode 1)

  (defun my-toggle-menubar()
    (interactive)
    (set-specifier menubar-visible-p
		   (not (specifier-instance menubar-visible-p))))
  
  (defun my-toggle-toolbar()
    (interactive)
    (set-specifier right-toolbar-visible-p
		   (not ( specifier-instance right-toolbar-visible-p))))
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
  (require 'scroll-in-place)
  (require 'filladapt)
  (add-hook 'text-mode-hook 'turn-on-filladapt-mode)
  (add-hook 'c-mode-hook 'turn-off-filladapt-mode)
  (require 'crypt)
  (autoload 'vhdl-mode "vhdl-mode" "VHDL Editing Mode" t)
  (setq auto-mode-alist (append '(("\\.vhdl?$" . vhdl-mode)) auto-mode-alist))
  )

;; inserts headers 
(load "/home/rradhakr/emacs/insert-header.elc")

(defun my-quietly-read-abbrev-file()
  (interactive)
  (setq abbrev-mode t)
  (quietly-read-abbrev-file "~/.abbrev_defs"))  

;; Automatic C/C++ Documentation 
;;(require 'doc)
;;(setq c++-mode-hook 
;;   '(lambda() (doc-setup)))
;;(put 'upcase-region 'disabled nil)

;; matches substrings as u switches buffers
(load "/home/rradhakr/emacs/iswitchb.elc")
(iswitchb-default-keybindings)

;; AUCTEX-9.9p
(require 'tex-site)
(setq reftex-plug-into-AUCTeX t)              ; Support for LaTeX Labels & References 
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; Emacs latex mode

;; Local Key maps
(define-key global-map "\C-cl" 'load-project) ;; load source files related to current project
(define-key global-map "\C-cp" 'my-switch-to-papaya) 
(define-key global-map "\C-cL" 'insert-latex2e-header)
(define-key global-map "\C-cP" 'insert-perl-file-header)
(define-key global-map "\C-cn" 'line-to-top-of-window)
(define-key global-map "\C-cm" 'my-quietly-read-abbrev-file)
(define-key global-map "\C-cs" 'shell)
(define-key global-map "\C-cv" 'send-invisible)
(define-key global-map "\C-cw" 'compare-windows)
(define-key global-map "\C-xz" 'repeat-complex-command)
(define-key global-map "\C-cj" 'goto-line)
(define-key global-map "\C-cc" 'compile)
(define-key global-map [(control ?x) (control ?m)] 'define-mail-alias)
(define-key global-map [(control button3)] 'popup-buffer-menu)
(define-key global-map "\C-xT" 'my-toggle-menubar)
(define-key global-map "\C-xR" 'my-toggle-toolbar)
(define-key global-map "\C-ca" 'map-switch-to-associated-buffer)
(define-key global-map "\C-xrd" 'delete-rectangle)
(define-key global-map  "\C-cr" 'revert-buffer)

;; for editing smv files
; (autoload 'smv-mode "smv-mode" "SMV mode." t)
; (add-to-list 'auto-mode-alist '("\\.smv$" . smv-mode))
; (add-to-list 'auto-mode-alist '("\\.ord$" . smv-ord-mode))
; (add-to-list 'completion-ignored-extensions ".ord")
; (add-to-list 'completion-ignored-extensions ".opt")

; for edition MONA files
;(autoload 'mona-mode "mona-mode" "MONA mode." t)
;(add-to-list 'auto-mode-alist '("\\.mona$" . mona-mode))
