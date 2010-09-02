;;;$file ~/emacs/sje-tex-extras.el$
;;;$date March 19, 1996$

;;; These are the extra functions for auctex that I have written.
;;; stephene@cogs.susx.ac.uk

;;; warning, only tested on emacs 19.27!

;;; add a few extra commands 
;;; note a temporary postscript file is created called /tmp/sje.ps
;;; if more than one user at a site uses this, it will probably need
;;; changing.

(setq sje-tex-commands-extra (list
	(list "ghostview.sje" "dvips -o /tmp/sje.ps  %s; ghostview /tmp/sje.ps &" 'TeX-run-command nil t)
	(list "dvips.sje" "dvips -o /tmp/sje.ps  %s" 'TeX-run-command nil t)
	(list "xdvi.sje" "xdvi %s &" 'TeX-run-command nil t)
))

;; add in the new commands.
(require 'tex)				;need this to define TeX-command-list
(setq TeX-command-list (append TeX-command-list sje-tex-commands-extra))

;; extra commands

(defun run-latex-on-file ()
  "Run latex on the current document."
  (interactive)
  (TeX-command "LaTeX" 'TeX-master-file))


(defun run-xdvi-on-file ()
  "Run xdvi on the current document."
  (interactive)
  (let ( (file))

    (if TeX-current-process-region-p
	;; then last tex command was a region.
	(progn 
	  (message "last tex on region")
	  (setq file 'TeX-region-file)
	  )
      ;; else last tex command on file.
      (progn
	(message "last tex on file")
	(setq file 'TeX-master-file)
	))
      
    (TeX-command "xdvi.sje" file)))




(defun run-dvips-on-file ()
  "Run dvips on the current document."
  (interactive)
  (let ( (file))
    
    (if TeX-current-process-region-p
	;; then last tex command was a region.
	(progn 
	  (message "last tex on region")
	  (setq file 'TeX-region-file)
	  )
      ;; else last tex command on file.
      (progn
	(message "last tex on file")
	(setq file 'TeX-master-file)
	))
      
    (TeX-command "dvips.sje" file)))

(defun run-ghostview-on-file ()
  "Run ghostview on the current document."
  (interactive)
  (let ( (file))
    
    (if TeX-current-process-region-p
	;; then last tex command was a region.
	(progn 
	  (message "last tex on region")
	  (setq file 'TeX-region-file)
	  )
      ;; else last tex command on file.
      (progn
	(message "last tex on file")
	(setq file 'TeX-master-file)
	))
      
    (TeX-command "ghostview.sje" file)))



(defun run-bibtex-on-file ()
  "Run bibtex on the current document."
  (interactive)
  (let ( (file))
    
    (if TeX-current-process-region-p
	;; then last tex command was a region.
	(progn 
	  (message "last tex on region")
	  (setq file 'TeX-region-file)
	  )
      ;; else last tex command on file.
      (progn
	(message "last tex on file")
	(setq file 'TeX-master-file)
	))
      
    (TeX-command "BibTeX" file)))




;; This next function was taken from the function 
;; (defun TeX-command-region (&optional old)
;; which can be found in tex-buf.el

(defun run-latex-on-region-old ()
  "run latex on the old region.
This is bound to C-c r."
  (interactive)
  (run-latex-on-region 'old))

(defun run-latex-on-region (&optional old)
  "Run TeX on the current region.

Query the user for a command to run on the temporary file specified by
the variable TeX-region.  If the chosen command is so marked in
TeX-command-list, and no argument (or nil) is given to the command,
the region file file be recreated with the current region.  If mark is
not active, the new text in the previous used region will be used.

If the master file for the document has a header, it is written to the
temporary file before the region itself.  The document's header is all
text before TeX-header-end.

If the master file for the document has a trailer, it is written to
the temporary file before the region itself.  The document's trailer is
all text after TeX-trailer-start."
  (interactive "P")
  (if (and (TeX-mark-active) (not old))
      (let ((begin (min (point) (mark)))
	     (end (max (point) (mark))))
	(if TeX-command-region-begin
	    ()
	  (setq TeX-command-region-begin (make-marker)
		TeX-command-region-end (make-marker)))
	(set-marker TeX-command-region-begin begin)
	(set-marker TeX-command-region-end end)))
  (if (null TeX-command-region-begin)
      (error "Mark not set"))
  (let ((begin (marker-position TeX-command-region-begin))
	(end (marker-position TeX-command-region-end)))
    (TeX-region-create (TeX-region-file TeX-default-extension)
		       (buffer-substring begin end)
		       (file-name-nondirectory (buffer-file-name))
		       (count-lines (save-restriction (widen) (point-min))
				    begin)))
  (TeX-command "LaTeX" 'TeX-region-file))

;;; extra keybindings
(defun extra-LaTeX-mode-hook ()
  "My hook for auc-tex's LaTeX-mode-hook. Do the binding of the extra keys"

  (define-key LaTeX-mode-map "\C-cf" 'run-latex-on-file)
  (define-key LaTeX-mode-map "\C-cr" 'run-latex-on-region)
  (define-key LaTeX-mode-map "\C-cv" 'run-xdvi-on-file)
  (define-key LaTeX-mode-map "\C-c\C-i" 'run-bibtex-on-file)
  (define-key LaTeX-mode-map "\C-cp" 'run-ghostview-on-file)
  (define-key LaTeX-mode-map "\C-c\C-p" 'run-dvips-on-file)
  (define-key LaTeX-mode-map "\C-cd" 'run-latex-on-region-old)
)
(add-hook 'LaTeX-mode-hook 'extra-LaTeX-mode-hook)


;;; change-graphic allows us to change between draft and final mode of
;;; the graphics package.  This is handy if you are including a lot of 
;;; pictures, and switch between modes fairly often.  To use it, you need
;;; the following text in the preamble:

;;;% sjegraphic 1
;;;\usepackage[draft]{graphicx}
;;;%\usepackage[]{graphicx}

;;; Using this function, will then toggle between the draft version and the
;;; final version, by changing the comments accordingly. So, calling it once
;;; will then make the line look like:
;;;
;;;% sjegraphic 2
;;;%\usepackage[draft]{graphicx}
;;;\usepackage[]{graphicx}
;;; 
;;; The number (1/2) indicates whether we are currently in draft or final
;;; mode  for convienience.

(defun change-graphic  ()
  "toggle the mode of the latex package graphics, to either draft or final.

"
  (interactive)
  (let ((draft) (pt1 nil) (line nil) beg end )
    
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^% sjegraphic \\([12]\\)$" )
      (setq draft (buffer-substring (match-beginning 1) (match-end 1)))
      ;(message "found %s" draft)
      (beginning-of-line)
      (setq pt1 (point))
      (kill-line)
      (if (string= "1" draft)
	  ;; then change to final
	    (setq line 2)
	;; else
	(setq line 1)
	)
      (insert (format "%% sjegraphic %d" line))
      (forward-line 1)
      (beginning-of-line)
      (if (= line 1 )
	  (progn
	    (delete-char 1)
	    (forward-line 1)
	    (beginning-of-line)
	    (insert "%")
	    (message "graphics now draft")
	    )
	;; else
	(progn
	  (insert "%")
	  (forward-line 1)
	  (beginning-of-line)
	  (delete-char 1)
	  (message "graphics now final")
	  )
	)
      )
    ))

(provide 'sje-tex-extras)
