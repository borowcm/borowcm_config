;; AUCTEX-9.9p
(setq load-path (cons "/home/ddelwork/rradhakr/auctex-9.9p" load-path))
(require 'tex-site)
(setq reftex-plug-into-AUCTeX t)              ; Support for LaTeX Labels & References 
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; Emacs latex mode 

;; 'TeX-master' variable is the most important variable that has to be
;; set correctly for you to compile your latex file inside Emacs
;; (using Auctex)

;; For my papers, the top-level latex files are always called
;; "print".tex. Uncomment this line and comment out lines 21-22, if
;; you are writing a paper.
(setq-default TeX-master "print") 

;; For my thesis, I have many subdirectories so prefer Auctex
;; prompting me which is the top-level file (with the directory path).
;; Uncomment the next lines 21-22 and comment out line 15, if you are
;; writing your thesis.
;(setq-default TeX-master nil) 
;(setq-default TeX-one-master "<none>") 

(setq TeX-parse-self t)
(setq TeX-auto-untabify nil) ; Tabs are not converted to spaces
;;(setq-default TeX-auto-save t) 

;; Reftex 
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-plug-into-AUCTeX t)
  
(defun insert-latex2e-header ()
   "Inserts a latex2e header"
   (interactive)
   (progn
   (insert-string "\\documentclass[11pt,twocolumn]{article}\n")
   (insert-string "\\usepackage{fullpage,epsfig}\n\n")
   (insert-string "\\oddsidemargin 0in\n")
   (insert-string "\\evensidemargin 0in\n")
   (insert-string "\\topmargin -1in\n")
   (insert-string "\\textwidth 6.4in\n")
   (insert-string "\\textheight 9in\n\n")
   (insert-string "\\title\{\}\n")
   (insert-string "\\author\{Vijay Sundaresan\}\n\n")
   (insert-string "\\begin{document}\n")
   (insert-string "\\maketitle\n\n\n")
   (insert-string "\\end{document}\n")
   (save-buffer))
)

;; Control-C Shift-L inserts the above lines into file 
(define-key global-map "\C-cL" 'insert-latex2e-header)
