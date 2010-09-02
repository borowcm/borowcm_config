;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		    Customization of LaTeX Mode			    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use specific customizations for ELIS-lab
(setq elis-user t)

;; Try to initialize AUC-TeX mode
(cond ((not (eq (locate-library "tex-site") nil))
       ; Tackle the problem that AUC TeX will produce error-messages if the 
       ; function `set-text-properties' is defined.
       (fset 'set-text-properties (symbol-function 'ignore))
       (require 'tex-site)))

;; Try to install a toolbar for AUC-TeX
(cond ((not (eq (locate-library "tex-toolbar") nil))
       (load-library "tex-toolbar")))

;; Commands to insert a figure (.eps) in LaTeX
(defun make-figure()
  "Insert figure latex commands"
  (interactive)
  (insert "\\begin{figure}")
  (newline)(insert "\t\\leavevmode")
  (newline)(insert "\t\\centering")
  (newline)(insert "\t\\epsffile{}")
  (newline)(insert "\t\\epsfxsize=\\textwidth")
  (newline)(insert "\t\\caption{}")
  (newline)(insert "\t\\label{}")
  (newline)(insert "\\end{figure}")
  (previous-line 4)
  (end-of-line)
  (backward-char 1)
)

;; Load all files for Ph.D.
(defun load-doct ()
  "Load most ann-files into buffers"
  (interactive)
  (find-file "~/latex/doctoraat/thesis.tex")
  (find-file "~/latex/doctoraat/h1.tex")
  (find-file "~/latex/doctoraat/h2.tex")
  (find-file "~/latex/doctoraat/h3.tex")
  (find-file "~/latex/doctoraat/h4.tex")
  (find-file "~/latex/doctoraat/h5.tex")
  (find-file "~/latex/doctoraat/h6.tex")
  (find-file "~/latex/inputs/abbrev.tex")
)

;; Load LaTeX-templates for letters and fax coversheets
(cond ((eq elis-user t)
      (load-library "latex-templates")))

;; Initialisations for LaTeX-mode
(add-hook 'LaTeX-mode-hook 
  (function
    (lambda () 
      (turn-on-auto-fill)
      (define-key LaTeX-mode-map '(f6)   'TeX-toolbar-run-LaTeX)
      (define-key LaTeX-mode-map '(f7)   'TeX-toolbar-file)
      (define-key LaTeX-mode-map '(f8)   'TeX-toolbar-ghostview)
      (define-key LaTeX-mode-map '(f9)   'TeX-toolbar-preview)
      (define-key LaTeX-mode-map '(f10)  'TeX-toolbar-print-default)
      (setq TeX-shell "/usr/bin/tcsh")
      (cond ((eq elis-user t)
	    (add-submenu nil '("Templates"
			 :filter file-menu-filter      
				      ; file-menu-filter is a function that 
				      ; takes one argument (a list of menu 
				      ; items) and returns a list of menu items
			 ["ELIS Letter (Ndl)"  elisletter-ndl-template t]
			 ["ELIS Fax    (Ndl)"  elisfax-ndl-template  t ]
			 ["ELIS Letter (Eng)"  elisletter-eng-template t]
			 ["ELIS Fax    (Eng)"  elisfax-eng-template  t ]
			 ["---" nil nil]
			 ["INW Letter (Ndl)"  petletter-ndl-template t]
			 ["INW Fax    (Ndl)"  petfax-ndl-template  t ]
			 ["INW Letter (Eng)"  petletter-eng-template t]
			 ["INW Fax    (Eng)"  petfax-eng-template  t ])
		   "Command")
            (set-menubar-dirty-flag)))
)))

;; Turns plain-tex-mode always into latex-mode
(add-hook 'plain-tex-mode-hook '(lambda () (latex-mode)))

;; Turn on auto-fill-mode for BibTeX
(add-hook 'bibtex-mode-hook 'turn-on-auto-fill)


