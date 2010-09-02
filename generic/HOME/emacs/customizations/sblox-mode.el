;;; sblox-mode.el --- Defining simple major mode for Sblox files 

;; Purpose: basic comment and font-lock support

;; Comment: Doesn't support multi-line comments '/*... */' yet
;; However for multi-line comments, do the following

;; M-x set-mark (C-spacebar) at the start of a block and move cursor
;; to end of block
;; M-x comment-region : to comment the entire block
;; C-u - M-x comment-region : to un-comment the entire block

;; Best viewed with the foll ~/.Xdefault values:
;; Emacs.Background:               black
;; Emacs.Foreground:               white
;; Emacs.font:                     -adobe-courier-medium-r-normal--14-100-100-100-m-90-iso8859-1

;; author: Rajesh Radhakrishnan

(require 'generic)
(require 'font-lock)

(defvar sblox-keyword-list
  (list
   "component" "compute" "interact" "interface" "map"
   "operation" "resources" "task"
   "inputs" "inout" "output" "locals" "state" "control" "nodata" 
   "combinational"  "sequential" 
   "carriers"
   "interaction" "operations" "integer" 
   "witness" 
   "for" "if" "then" "else" "while" 
   "begin" "end" 
   "after" "at" "case" "flags" 
   "skip"  "raise" "lower"
   "high" "low"
   "of" "others" 
   "until" "using" 
    )
  "Keywords in Sblox.")

(define-generic-mode 'sblox-mode
  (list "--")
  sblox-keyword-list
  (list

   ;; matches carrier decls
   (list "\\([_A-Za-z0-9]+\\[[A-Za-z0-9]+\\]\\)"   '(1 font-lock-variable-name-face))

   ;; matches get/put decls
   (list "\\<\\(put\\|get\\)\\>[ \t]*\\(\\sw+\\)?"
	 '(1 font-lock-builtin-face) '(2 font-lock-function-name-face nil t))

   ;; matches operator call 'op()'
   (list "\\<\\([A-Za-eg-z0-9_-]+\\)\\>[ \t]*\\s(\\(\\sw+\\)?"
	 '(1 font-lock-type-face)) ;; the seq [a-eg-z] is so that "if"
				   ;; isn't included
   ;; matches 'transfer' call
   (list "\\<\\(transfer\\)\\>"
	 '(1 font-lock-reference-face))
   
   ;; matches #include's
   (list "^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)"
	  '(1 font-lock-string-face))   

   ;; matches restriction arg 'm:' in "m: m<n"
   (list "[ \t]*\\([A-Za-z_0-9]+\\s-*:\\)" 
	 '(1 font-lock-function-name-face))
   ;;;
   )
    (list "\\.[sS][bB][lL][oO][xX]$")
  (list 'sblox-mode-setup-function)
  "Mode for Sblox files")

(defun sblox-mode-setup-function () 
  (interactive)
  (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
  (setq font-lock-use-fonts t)
  (setq font-lock-use-colors t)
  (setq font-lock-use-maximal-decoration t)
)

;; The colors are set here. Change them to your liking. 
; (custom-set-faces
;  '(font-lock-comment-face ((((class color) (background dark)) (:italic t :foreground "lightgrey"))))
;  '(font-lock-keyword-face ((((class color) (background dark)) (:bold t :italic t :foreground "Cyan"))))
;  '(font-lock-constant-face ((t (:bold t :italic t :foreground "red3"))))
;  '(font-lock-reference-face ((t (:bold t :italic t :foreground "tan"))))
;  '(font-lock-type-face ((t (:bold t :foreground "orange"))))
;  '(italic ((t (:italic t :foreground "tan"))))
;  '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "yellow3"))))
;  '(font-lock-function-name-face ((((class color) (background dark)) (:italic t :foreground "orange"))))
;  '(bold ((t (:bold t :foreground "orange"))))
;  '(font-lock-builtin-face ((((class color) (background dark)) (:italic nil :foreground "lightgreen")))))

(provide 'sblox-mode) ;; that'all folks!
