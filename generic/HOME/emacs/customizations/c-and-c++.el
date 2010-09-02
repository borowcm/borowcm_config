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
                                   (block-close       . 0)
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
  (define-key c-mode-base-map "\C-q" 'indent-all)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)

(defvar date-format "%m-%d-%Y"
  "The format used to display dates when using the \\date command.")

(defun insert-cvs-id ()
  "Inserts CVS Id information"
  (interactive)
  (progn
    (insert-string (concat "// $Id:" (buffer-name) "$"))(newline)
    (save-buffer))
)

(defun insert-copyright-header () 
  "Inserts copyright information into a .hh file"
  (interactive)
  (progn 
    (insert-string "// Copyright (c) The University of Cincinnati. All rights reserved.")(newline)
    (insert-string "// UC MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF")(newline)
    (insert-string "// THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED")    (newline)
    (insert-string "// TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A")(newline)
    (insert-string "// PARTICULAR PURPOSE, OR NON-INFRINGEMENT.  UC SHALL NOT BE LIABLE")(newline)
    (insert-string "// FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING,")(newline)
    (insert-string "// RESULT OF USING, MODIFYING OR DISTRIBUTING THIS SOFTWARE OR ITS")(newline)
    (insert-string "// DERIVATIVES.")(newline)
    (insert-string "// By using or copying this Software, Licensee agrees to abide by the")(newline)
    (insert-string "// intellectual property laws, and all other applicable laws of the")(newline)
    (insert-string "// U.S., and the terms of this license.")(newline)
    (insert-cvs-id) (newline)
    (insert-string "/*!") (newline)    
    (insert-string (concat "\\file " (buffer-name)))(newline) (newline)
    (insert-string "\\brief")(newline)(newline)
    ;;(insert-string "<long description>")(newline)(newline)
    (insert-string (concat "\\author: " (user-full-name)))(newline)
    (insert-string (concat "\\date: " (format-time-string date-format)))(newline)
    (insert-string "*/")(newline)    
    (save-buffer))
)
   
(defun message-replace-chars-in-string (string from to)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))   ;Copy string.
        (len (length string))
        (idx 0))
    ;; Replace all occurrences of FROM with TO.
    (while (< idx len)
      (when (= (aref string idx) from)
        (aset string idx to))
      (setq idx (1+ idx)))
    string))

;; author : Evgeny Roubinchtein <eroubinc@u.washington.edu>
(defun my-insert-hh-file-header (&optional upcasep)
  "Inserts ifdefs into the .hh file
   With prefix, uppercase the #ifdef'ed name
   BUGS: does not handle narrowing."
  (interactive "P")
  (save-excursion
    (let* ((name (message-replace-chars-in-string
		  (file-name-nondirectory (buffer-file-name))
		  ?. ?_ ))
	   (nm (if (null upcasep) name (upcase name)))
	   (buf-head (format "#ifndef _%s_\n#define _%s_\n" nm nm))
	   (buf-tail (format "\n#endif // _%s_ " nm)))
      (goto-char (point-min)) (insert buf-head)
      (goto-char (point-max)) (insert buf-tail))))

;; added this func to call the above func with uppercase as default
(defun insert-hh-file-header ()
  (interactive)
  (my-insert-hh-file-header 'upcasep))

(define-key global-map "\C-cH" 'insert-hh-file-header) 
(define-key global-map "\C-cC" 'insert-copyright-header) 

;; Load C/C++ Auto-Switch package. Automatically loads/switches
;; between .cc/.hh, .c/.h files
(autoload 'map-switch-to-associated-buffer "switch-assoc"
  "Switch to an associated buffer" t)

(defun my-insert-class-skeleton()
  "Inserts class declaration into the .hh file"
  (interactive)
  (setq name (file-name-nondirectory (buffer-file-name)))
  (setq len (length name))
  (setq width (- len 3))
  (setq classname (substring name 0 width))
  (insert-string (concat "class " classname " {"))(newline)(newline)
  (setq current-line (what-line))
  (insert-string "public:")(newline)
  (insert-string (concat "    " classname "();"))(newline)
  (insert-string (concat "    ~" classname "();"))(newline)
  (insert-string (concat "    "classname "(const " classname "& );"))(newline)
  (insert-string (concat "    " classname "& operator=(const " classname "& );"))(newline)(newline)
  (insert-string "protected:")(newline)(newline)
  (insert-string "private:")(newline)(newline)
  (insert-string "};")(newline)
  (goto-line current-line) (end-of-line))

(global-set-key "\C-cc" 'my-insert-class-skeleton)


(define-key global-map "\C-ca" 'map-switch-to-associated-buffer)

;;; Michael's Function Definitions
;;        *** --------------------------------------------------
;;;  c-return           ;;; In c: indent & open indented new line
(fset 'indent-all "\C-xh\C-[\C-\\")
(defun open-new-line( ) (interactive) (end-of-line) (newline-and-indent))
(defun c-return( ) (interactive) (c-indent-line) (newline-and-indent))
;;; c-mode
(add-hook 'c-mode-hook
   '(lambda() 
        (local-set-key [13] 'c-return)        ;;; RET with automatic indent
;;        (local-set-key [16] 'indent-all)      ;;; Ctrl-p pretty-prints file
    )
)
;;; c++-mode
(add-hook 'c++-mode-hook
   '(lambda() 
        (local-set-key [13] 'c-return)        ;;; RET with automatic indent
;;        (local-set-key [16] 'indent-all)      ;;; Ctrl-p pretty-prints file
    )
)

;; automatically switch 'abbrev-mode' on 
(add-hook 'c++-mode-hook
	  (function
	   (lambda ()
	     (setq abbrev-mode t))))

;; Sun Workshop
;;(setq load-path (append load-path '("/opt/SUNWspro/WS6/lib")))
;(require 'workshop)

(setq compilation-read-command t)
