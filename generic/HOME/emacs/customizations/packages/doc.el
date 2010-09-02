;;; doc.el --- First try of writing a documentation assitant.
;; Author: Peter Frey
;; The Author takes no guarantee for completeness or correctness of 
;; this code. There is no warranty against any loss or damage.
;;  
;; Please send suggestions, questions and bug reports to
;;    pfrey@ece.uc.edu 
;; The newest release is avialable through http://www.ece.uc.edu/~pfrey

;; modified by Rajesh, for including C++ style comments
;; don't forget to change the value of 'doc-string' for your own copyright info

(provide 'doc)

(defun doc-setup ()
  (interactive)
  (define-key (current-local-map) [(control ?c) h]    'doc-header)
  (define-key (current-local-map) [(control ?c) (control ?f)] 'doc-function)
  (define-key (current-local-map) [(control ?c) (control ?d)] 'doc-field)
  (define-key (current-local-map) [(control ?c) (control ?w)] 'doc-auto-wrap)
  (define-key (current-local-map) [(control ?c) (control ?r)] 'doc-frame)
)

;;; Functions necessary for the header description

;; change this string 
(defvar doc-string "Copyright (c) 1998-1999 Ohio Board of Regents and the University of Cincinnati.  All rights reserved." "Documentation string in header")

(defvar doc-wide 74 "Length of a comment including boarder")
(defvar doc-arg-indent 5 "This is the indentation of the fields in the function header. This value has to be less than 75 and more than 1")


(defun doc-sep ()
  "Printout separator line"
  (insert "\n" (make-string (- doc-wide 2) ?\ ) "\n" )
)

(defun doc-empty ()
  "Printout an empty line"
  (insert "//" (make-string (- doc-wide 4) ?\ ) "\n" )
)

(defun doc-print (str)
  "Print the string str as a C comment"
  (let ((outstr str))
    ;; The string is subdevided if it is too long for one line
    (while (> (length outstr) (- doc-wide 6))
      (let ((cutstr (substring outstr 0 (- doc-wide 6))) )
	(string-match ".*[,:\) ]" cutstr)
	(if (> (match-end 0) 0)
	    (progn 
	      (doc-print-checked (substring cutstr 0 (match-end 0)))
	      (setq outstr (substring outstr (match-end 0) (length outstr)))
	      )
	  )
	)
      )
    (doc-print-checked outstr)
    )
)

(defun doc-print-checked (str)
  "This function is called when it is guaranteed that the string matches into the specified length This function adds the sides"
  (let ((str-start (format "// %s" str)))
    ;; needs to be improved
    (insert
     (concat 
      str-start
      (make-string (- doc-wide (+ (length str-start) 3)) ?\ )
      " \n"))  
    )
)

(defun doc-header ()
  "This function is generating the header documentation with the fields: Author, Date of creation, Last changed, Last changed by"
  (interactive)
  (let ( (pos (point-marker)) )
    (goto-char (point-min))
    (if (search-forward "// File: " nil t)
	;; test if only update has to be done
	(doc-header-update)
      ;; create first time copy
      (doc-print (concat "File: " (buffer-name)))
;;      (doc-sep)
      (doc-print (concat "Author:" (user-full-name)))
      (doc-print (concat "Date of creation: " (current-time-string)))
;;      (doc-print (concat "Last changed:     " (current-time-string)))
;;      (doc-print (concat "Last changed by:  " (user-full-name)))
      (doc-empty)
      (doc-print doc-string)
      (doc-sep)
      )
    (goto-char (marker-position pos))
    )
)

(defun doc-header-update ()
  "updates the time string and the last changed name in the doc-header"
  (goto-char (point-min))
  (if (search-forward "// Last changed:   " nil t)
      (progn 
	(beginning-of-line)
	(kill-line 1)
	(doc-print (concat "Last changed:     " (current-time-string)))
	)
    )
  (if (search-forward "// Last changed by:  " nil t)
      (progn 
	(beginning-of-line)
	(kill-line 1)
	(doc-print (concat "Last changed by:  " (user-full-name)))
	)
    )
)


;;; Restriction of output

(defun doc-auto-wrap ()
  "This function breaks the output of a header into peaces such that it is not going beond the predefined length"
  (interactive)
  (let ((start 0) (first (point)))
    (beginning-of-line)
    (if (looking-at "/\\/")
	(progn 
	  (forward-line -1)
	  (while (and (looking-at "/\\/")
		      (> (point) (point-min)))
	    (forward-line -1))
	  (if (> (point) (point-min))
	      (forward-line 1))
	  (setq first (point))
	  (while (looking-at "/\\/")
	    (setq start (point))
	    (end-of-line)
	    (if (and (<= (current-column) doc-wide)
		     (progn (goto-char (- (point) 2))
			    (if (looking-at "\\/")
				t
			      (end-of-line)
			      (insert "  ")
			      nil)))
		()
	      (delete-char -3)
	      (while (char-equal (char-after (- (point) 1)) ?\ )
		(delete-char -1))
              (goto-char start)
	      (delete-char 3)
	      (kill-line)
	      (kill-line)
	      (doc-print (car (cdr kill-ring)))  
	      (forward-line -1)
	      )
	    (beginning-of-line)
	    (forward-line 1)
	    )
	  (goto-char first))
      (goto-char first)
      (error 
       "Check the pointer position. It seems not to be in a documentation area.")
      )))



;;; Field stuff

(defun doc-field ()
  "This function goes to the next documentation field in the current funciton description"
  (interactive)
  (let ((pos (point)) (start 0))
    (message "Please fill all fields. C-c C-d next field.")

    (beginning-of-line)
    (if (looking-at "//")
	(progn 
	  ;; go to the beginning
	  (forward-line -1)
	  (while (and (looking-at "//")
		      (> (point) (point-min)))
	    (forward-line -1))
	  (if (> (point) (point-min))
	      (forward-line 1))
	  (setq start (point))
	  ;; go to the end
	  (while (looking-at "//")
	    (forward-line 1))
	  ;; find last field
	  (if (and (search-backward "Function description: " nil t)
		   (> (point) start))
	      (progn
		(search-forward "Function description: " nil t)
		(if (>= pos (point))
		    (error "No more fields")
		  (if (and (search-backward 
			    "The following parameter(s) are used: " nil t)
			   (> (point) start))
		      (progn 
			(beginning-of-line)
			(while (> pos (point))
			  (forward-line 1))
			(while (not (or 
			     (looking-at 
			      (concat "//" 
				      (make-string doc-arg-indent ?\ ) 
				      "[^\n]+ : " ))
			     (looking-at "// Function description: ")))
			  (forward-line 1))
			(goto-char (match-end 0)))
		    (while (<= (point) start)
			(search-forward "// Function description: " nil t))))) 
	    (error "Not in a function header"))
	  )
      (error "Not in a function header"))))

;;; Function Documentation


(defun doc-function () "Documentation of a function header. It is
important that the current possition of the cursor in the buffer is
before the function definition. If it is in a possible already
existing function header the header is updated. If the cursor is on
the line of the definition a new header is created in any way. A new
function header will be inserted on the line where the cursor
is. ATTENTION, there is no check if the pointer is at the right place"
(interactive) (let ( (pos 0) (buf 0) (filebuf 0) (tmp nil) (str "")
(para 0) (has-return nil) (start 0) (end 0) (update-doc t) (oldbuf 0)
)

    (beginning-of-line)
    ; test if the cursor is in the function header
    (if (looking-at "//")
	(progn 
	  (forward-line -1)
	  (while (and (looking-at "//")
		      (> (point) (point-min)))
	    (forward-line -1))
	  (if (> (point) (point-min))
	      (forward-line 1))
	  (setq start (point))
	  (while (looking-at "//")
	    (forward-line 1))
	  (setq end (point))
	  (if (and (search-backward "Function description: " nil t)
		   (> (point) start))
	      (progn
		(setq update-doc nil)
		(kill-region start end)
		(setq filebuf (current-buffer))
		(setq oldbuf (generate-new-buffer "takeapart"))
		(set-buffer oldbuf)
		(yank)
		(set-buffer filebuf)
		))))

    ; search for end of function definition
    (setq pos (point))
    (if (search-forward "{" nil t)
	(progn
	  (backward-char)
	  ;create copy of function in a sepparate buffer
	  (copy-region-as-kill pos (point))

	  (setq filebuf (current-buffer))
	  (setq buf (generate-new-buffer "takeapart"))
	  (set-buffer buf)
	  (yank)
	  (doc-remove-comments)
	  (setq str (buffer-string))
	  (set-buffer filebuf)

	  (message "Please fill all fields. C-c C-d next field.")

	  (goto-char pos)
	  (doc-sep)
	  
	  ; insert function definition
;;	  (doc-classdef str)
;;	  (doc-sep)

	  ; insert generic description
	  (if (string-equal (doc-class-name buf) "")
					; just a function
	      (progn
;;		(doc-print
;;		 (concat "This is the definition of the function '"
;;			 (doc-function-name buf) "'."))
;;		(setq has-return t)
		)

	    (setq tmp (doc-class-name buf))
	    (cond 
	     ((doc-constructor buf)
	      ; constructor definition
	      (doc-print 
	       (concat
		"This is the definition of the constructor of the class '" 
		tmp "'.")))
	     ((doc-destructor buf)
	      ; destructor definition
	      (doc-print 
	       (concat 
		"This is the definition of the destructor of the class '"
		tmp "'.")))
	     (t (doc-print 
		 ; method definition
		 (concat 
		  "This is the definition of the method '" 
		  (doc-function-name buf)
		  "' of the class '" tmp "'."))
		(setq has-return t)
		)
	     )
	    )
	  
	  ; insert attributes of the function
	  (if (setq para (doc-attributes buf))
	      (progn 
		(doc-print "It is defined with the following attribute(s): ")
		(while para
		  (doc-print (concat (make-string doc-arg-indent ?\ ) 
				     (car para) ))
		  (setq para (cdr para))
		  )
		)
	    )
;;	  (doc-empty)

	  ; insert parameter declaration
	  (if (setq para (doc-parameters buf))
	      (progn 
	;;	(doc-print "The following parameter(s) are used: ")
		(while para
		  (if (not update-doc)
		      ; take over previous definition if there is one
		      (let ((start 0))
			(set-buffer oldbuf)
			(goto-char (point-max))
			(if (search-backward (concat "// "
					      (make-string doc-arg-indent ?\ ) 
					      (car para) " :") nil t)
			    (progn
			      (setq start (point))
			      (forward-line 1)
			      (search-forward (concat 
					       "// " 
					       (make-string doc-arg-indent
							    ?\ )) nil t)
			      (beginning-of-line)
			      (kill-region start (point))
			      (set-buffer filebuf)
			      (yank)
			      )
			  ; no definition was given
			  (set-buffer filebuf)
;;			  (doc-print (concat (make-string doc-arg-indent ?\ ) 
;;					     (car para) " : "))
			))
		    ; it is first time not an update
;;		    (doc-print (concat (make-string doc-arg-indent ?\ ) 
;;				       (car para) " : "))
		    (setq para (cdr para))
		    ))
;;		(doc-empty)
		)
;;	    (doc-print "No parameters.")
;;	    (doc-empty)
	    )

	  ; describe returnvalue
;;	  (if (null has-return)
;;	      ()
;;	    (doc-print (concat "The returnvalue has the type '"
;;			       (doc-return-value buf)
;;			       "'."))
;;	    (doc-empty))


	  ; descripe Function description
	  (if (not update-doc)
	      ; Take over previous definition if there is one
	      (progn
		(set-buffer oldbuf)
		(goto-char (point-max))
		(search-backward "// Function description:" nil t)
		(setq start (point))
		(search-forward (concat "//" (make-string (- doc-wide 4)
							  ?\ ) ""))
		(beginning-of-line)
		(kill-region start (point))
		(set-buffer filebuf)
		(yank)
		(kill-buffer oldbuf)
		)
	    ; declare first time description
	    (doc-print "Function description:")
	    )
;;	  (doc-empty)
	
	  ; describe members which are initialized
	  (if (setq para (doc-members buf))
	      (progn 
		(doc-print 
		 "The following initializations of members are performed: ")
		(while para
		  (doc-print (concat (make-string doc-arg-indent ?\ )
				     (car para)))
		  (setq para (cdr para))
		  )
		(doc-empty)
		)
	    )
;;	  (doc-sep)
	  
	  (kill-buffer buf)
	  
	  ; go to first field
	  (forward-line -1)
	  (while (and (looking-at "/\\*")
		      (> (point) (point-min)))
	    (forward-line -1))
	  (if (> (point) (point-min))
	      (forward-line 1))
	  (doc-field)
	  )
      )
    )
  )


(defun doc-classdef (str)
  "Create pseudo class definition (no parameters)"
  (let ((str1 "") (pos 0) (end 0))
    (string-match "[^(]+" str)
    (setq str1 (substring str (match-beginning 0) (match-end 0)))
    (if (string-equal 
	 (substring str (+ (match-end 0) 1) (+ (match-end 0) 2)) ")")
	(doc-print (concat str1 "()"))
      (doc-print (concat str1 "(...)"))
      )
    )
)

(defun doc-constructor (buf)
  "Check if it is a constructor"
  (string-match (doc-function-name buf) (doc-class-name buf) )
)

(defun doc-destructor (buf)
  "Check if it is a destructor"
  (let ((fct (doc-function-name buf)))
    (if (string-equal (substring fct 0 1) "~")
	(string-match (substring fct 1 nil) (doc-class-name buf) )
      )
    )
)

(defun doc-return-value (buf)
  "Determine the return value"
  (let ((oldbuf (current-buffer))(retval "")(pos 0)(name 0))
    (set-buffer buf)
    
    (goto-char (point-min))
    (if (re-search-forward "^[^\(]*volatile[^\(]*\(" nil t)
	(setq retval (concat retval "volatile "))
      )
    
    (goto-char (point-min))
    (if (re-search-forward "^[^\(]*const[^\(]*\(" nil t)
	(setq retval (concat retval "const "))
      )

    (goto-char (point-min))
    (or (re-search-forward "\\([_a-zA-z][_a-zA-z0-9<>]*::\\)+" nil t)
	(re-search-forward "~?[_a-zA-z][_a-zA-z0-9<>]*\(" nil t))
    (setq pos (match-beginning 0))
    (goto-char pos)
    (if (re-search-backward 
	 "[^_a-zA-z][_a-zA-z][_a-zA-z0-9]*\\( \\*\\|\\*\\| \&\\|\&\\)?"
	 nil t)
	(progn 
	  (setq name (buffer-substring (+ (match-beginning 0) 1)
				       (match-end 0) ))
	  (if (string-equal name "template")
	      (setq retval (concat retval "int"))
	    (if (string-match name retval)
		(setq retval (concat retval "int"))
	      (setq retval (concat retval name))
	      )
	    )
	  )
      (goto-char pos)
      (if (re-search-backward 
	 "^[_a-zA-z][_a-zA-z0-9]*\\( \\*\\|\\*\\| \&\\|\&\\)?" nil t)
	  (progn 
	    (setq name (buffer-substring (match-beginning 0)
					 (match-end 0) ))
	    (if (string-equal name "template")
		(setq retval (concat retval "int"))
	      (if (string-match name retval)
		  (setq retval (concat retval "int"))
		(setq retval (concat retval name))
		)
	      )
	    )
	(setq retval (concat retval "int"))
	)
      )

    (set-buffer oldbuf)
    retval
    )
)


(defun doc-parameters (buf)
  "Determine the parameters they are returned in a list"
  (let ((oldbuf (current-buffer))(parameters (list))(para "")(notdone t)
	(pos 0))
    (set-buffer buf)
    
    (goto-char (point-min))
    (search-forward "(" nil t)
    (if (looking-at ")")
       ()
      (while (and notdone
		  (re-search-forward "[^,\)]+[,\)]" nil t))
	(setq para (buffer-substring (match-beginning 0)
				     (- (match-end 0) 1)))
	(setq pos (match-beginning 0))
	(while (not (doc-balanced para))
	  (re-search-forward "\)" nil t)
	  (setq para (buffer-substring pos (match-end 0)))
	  )
	(if (string-equal 
	     (buffer-substring (- (match-end 0) 1) (match-end 0)) "\)")
	    (setq notdone nil)
	  )
	(setq parameters (append parameters (list para)))
	(setq para "")
	)
      )
    (set-buffer oldbuf)
    parameters
    )
)

(defun doc-members (buf)
  "Determine the members which are initialized. They are returned in a list"
  (let ((oldbuf (current-buffer))(members (list))(para "")(notdone t)(pos 0))
    (set-buffer buf)
    
    (goto-char (point-min))
    (if (re-search-forward ")[^:]*:" nil t)
	(progn 
	  (goto-char (match-end 0))
	  (setq pos (match-end 0))
	  (while notdone
	    (if (re-search-forward "[^,]+," nil t)
		(progn 
		  (setq para (buffer-substring pos
					       (- (match-end 0) 1)))
                  (while (not (doc-balanced para))
		    (re-search-forward "\)" nil t)
		    (setq para (buffer-substring pos (match-end 0)))
		    )
		  (setq members (append members (list para)))
		  (if (looking-at "$")
		      (setq notdone nil))
		  )
	      (goto-char pos)
	      (if (re-search-forward "[^,]+$" nil t)
		  (progn
		    (setq para (buffer-substring (match-beginning 0)
						 (match-end 0)))
		    (setq members (append members (list para)))
		    (setq notdone nil)
		    )
		(setq notdone nil)
		)
	      )
	    (goto-char (match-end 0))
	    (setq pos (- (match-end 0) 1))
	    (setq para "")
	    )
	  )
      )
    (set-buffer oldbuf)
    members
    )
)

(defun doc-balanced (str)
  "checks if the paranthesis are ballanced in the string argument"
  (let ((open 0) (close 0) (start 0) (data (match-data)))
    (while (string-match "[\[\(\{]" str start)
      (setq start (+ 1 (match-beginning 0)))
      (setq open (+ 1 open))
      )
    (setq start 0)
    (while (string-match "[\]\)\}]" str start)
      (setq start (+ 1 (match-beginning 0)))
      (setq close (+ 1 close))
      )
    (store-match-data data)
    (equal open close)
    )

  )

(defun doc-attributes (buf)
  "This function determines the attributes of the function, they are returned in a list"
  (let ((oldbuf (current-buffer))(attributes (list)) )
    (set-buffer buf)
    
    (goto-char (point-min))
    (if (re-search-forward "^[^\(]*template[^\(]*\(" nil t)
	(setq attributes (append attributes (list "template")))
      )

    (goto-char (point-min))
    (if (re-search-forward "^[^\(]*inline[^\(]*\(" nil t)
	(setq attributes (append attributes (list "inline")))
      )	

    (goto-char (point-min))
    (if (re-search-forward "^[^\(]*\([^\)]*\)[^\(\)]*const" nil t)
	(setq attributes (append attributes (list "const")))
      )	

    (goto-char (point-min))
    (if (re-search-forward "^[^\(]*\([^\)]*\)[^\(\)]*volatile" nil t)
	(setq attributes (append attributes (list "volatile")))
      )	

    (set-buffer oldbuf)
    attributes
    )
)

(defun doc-class-name (buf)
  "This function determines a possible classname which is returned"
  (let ((oldbuf (current-buffer))(classname ""))
    (set-buffer buf)
    
    (goto-char (point-min))
    (if (re-search-forward "\\([_a-zA-Z][_a-zA-Z0-9\<\>]*::\\)+" nil t)
	(progn
	  (setq classname (buffer-substring (match-beginning 0)
					    (- (match-end 0) 2)))
	  )
      )
    (set-buffer oldbuf)
    classname
    )
)

(defun doc-function-name (buf)
  "This function determines the functionname"
  (let ((oldbuf (current-buffer))(functionname ""))
    (set-buffer buf)
    
    (goto-char (point-min))
    (if (search-forward "operator" nil t)
	(progn 
	  (goto-char (point-min))
	  (if (re-search-forward "operator\\([^(]*\\|()\\)\(" nil t)
	      (setq functionname (buffer-substring (match-beginning 0)
						   (- (match-end 0) 1)))
	    )
	  )
      (goto-char (point-min))
      (if (re-search-forward "~?[_a-zA-Z][_a-zA-Z0-9]*\(" nil t)
	  (setq functionname (buffer-substring (match-beginning 0)
					       (- (match-end 0) 1)))
	)
      )
    (set-buffer oldbuf)
    functionname
    )
)

(defun doc-in-comment (start)
  "This function returns true if the number of appostropies (\") before the current possition is odd. If it is even it returns nil. This is similar to the detection if the cursor is currently in a comment or not."
  (let ( (count 0) (pos (point) ) )
    (goto-char start)
    (while (search-backward "\"" nil t)
      (setq count (+ count 1))
      )
    (goto-char pos)
    (if (equal (% count 2) 0)
	nil
      t)
    )
)

(defun doc-remove-comments ()
  "This function cleans up the class definition from unnecessary comments, blanks and newlines"
  ;; replace comments!
  (goto-char (point-min))
  (let ((count 0) (end 0) (start 0) (pos 0) 
	(fillchar (list ?\( ?\) ?: ?> ?< ?~ ?, ?\ )) )
    (while (re-search-forward "\\(/\\*.*\\*/\\|//.*$\\|[ \t\n]+\\)" nil t)
      (setq end   (match-end 0))
      (setq start (match-beginning 0))
      (goto-char start)
      (if (doc-in-comment start)
	  (goto-char (1+ start))
	(kill-region start end)
	(insert " ")
	)
      )

    ;; remove whitespaces
    (goto-char (point-min))
    (while (re-search-forward "[\ \n\t]+" nil t)
      (setq end   (match-end 0))
      (setq start (match-beginning 0))
      (if (doc-in-comment (point))
	  (goto-char (1+ (point)))
	(kill-region start end)
	(insert " ")
	)
      )
            
    ;; remove unnecessary spaces arround special characters
    (goto-char (point-min))
    (while (search-forward " " nil t)
      (setq end (point))
      (if (doc-in-comment end)
	  (goto-char end)
	(if (or (memq (char-after end) fillchar)
		(memq (char-after (- end 2)) fillchar))
	    (progn 
	      (goto-char end)
	      (delete-char -1))
	  )
	(goto-char end)
	)
      )
    
    ;; remove whitespace at the beginning
    (goto-char (point-min))
    (if (char-equal (char-after 1) ?\ )
	(delete-char 1))
    )
)

(defun doc-frame ()
  "Write a command frame"
  (interactive)
  (doc-empty)
  (forward-line -3)
  (forward-char 3)
)
