;;; -*-Emacs-Lisp-*-
;;;%Header
;;; Partial completion mechanism for GNU Emacs.  Version 3.02
;;; Copyright (C) 1990, 1991 Chris McConnell, ccm@cs.cmu.edu.
;;; Thanks to Bjorn Victor for suggestions, testing, and patches for
;;; file completion. 

;;; This file is part of GNU Emacs.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; When loaded, this file extends the standard completion mechanisms
;;; so that they perform pattern matching completions.  There is also
;;; an interface that allows it to be used by other programs.  The
;;; completion rules are:
;;;
;;; 1) If what has been typed matches any possibility, do normal
;;; completion. 
;;;
;;; 2) Otherwise, generate a regular expression such that
;;; completer-words delimit words and generate all possible
;;; matches.  If completer-use-words is T, a match is unique if it is
;;; the only one with the same number of words.  If
;;; completer-use-words is NIL, a match is unique if it is the only
;;; possibility.  If you ask the completer to use its best guess, it
;;; will be the shortest match of the possibilities unless
;;; completer-exact is T.
;;;
;;; 3) For filenames, if completer-complete-filenames is T, each
;;; pathname component will be individually completed.
;;;
;;; SPACE, TAB, LFD, RET, and ? do normal completion if possible
;;; otherwise they do partial completion.  In addition, C-DEL will
;;; undo the last partial expansion or contraction.  M-RET will always
;;; complete to the current match before returning.  This is useful
;;; when any string is possible, but you want to complete to a string
;;; as when calling find-file.  The bindings can be changed by using
;;; completer-load-hook.
;;;
;;; Modes that use comint-dynamic-complete (like cmushell and ilisp)
;;; will also do partial completion as will M-tab in Emacs LISP.
;;;
;;; Examples:
;;; a-f     auto-fill-mode
;;; b--d    *beginning-of-defun or byte-recompile-directory
;;; ~/i.e   *~/ilisp.el or ~/il-el.el or ~/ilisp.elc
;;; /u/mi/  /usr/misc/
;;;

;;;%Globals
;;;%%Switches
(defvar completer-load-hook nil
  "Hook called when minibuffer partial completion is loaded.")

(defvar completer-disable nil
  "*If T, turn off partial completion.  Use the command
\\[completer-toggle] to set this.")

(defvar completer-complete-filenames t
  "*If T, then each component of a filename will be completed.")

(defvar completer-use-words t
  "*If T, then prefer completions with the same number of words as the
pattern.")

(defvar completer-words "---. <" 
  "*Delimiters used in partial completions.  It should be a set of
characters suitable for inclusion in a [] regular expression.")

(defvar completer-exact nil
  "*If T, then you must have an exact match.  Otherwise, the shortest
string that matches the pattern will be used.")

;;;%%Internal
(defvar completer-last-pattern ""
  "The last pattern expanded.")

(defvar completer-message nil
  "T if temporary message was just displayed.")

;;;%Utilities
(defun completer-message (message &optional point)
  "Display MESSAGE at optional POINT for two seconds."
  (setq point (or point (point-max))
	completer-message t)
  (let ((end
	 (save-excursion
	   (goto-char point)
	   (insert message)
	   (point)))
	(inhibit-quit t))
    (sit-for 2)
    (delete-region point end)
    (if quit-flag
	(setq quit-flag nil
	      unread-command-char 7))))

;;;%%Regexp
(defun completer-regexp (string delimiters)
  "Convert STRING into a regexp with words delimited by characters in
DELIMITERS." 
  (let* ((delimiters (concat "[" delimiters "]"))
	 (limit (length string))
	 (pos 0)
	 (regexp "^"))
    (while (and (< pos limit) (string-match delimiters string pos))
      (let* ((begin (match-beginning 0))
	     (end (match-end 0))
	     (delimiter (substring string begin end)))
	(setq regexp 
	      (format "%s%s[^%s]*%s" 
		      regexp
		      (regexp-quote (substring string pos begin))
		      delimiter
		      (regexp-quote delimiter)))
	(setq pos end)))
    (if (<= pos limit)
	(setq regexp (concat regexp 
			     (regexp-quote (substring string pos limit)))))))

;;;
(defun completer-words (regexp string &optional limit)
  "Return the number of words matching REGEXP in STRING up to LIMIT."
  (setq limit (or limit 1000))
  (let ((count 1)
	(pos 0))
    (while (and (string-match regexp string pos) (<= count limit))
      (setq count (1+ count)
	    pos (match-end 0)))
    count))

;;;%Matcher
(defun completer-match (string choices delimiters use-words)
  "Return the best match of STRING in CHOICES with DELIMITERS between
words and T if it is unique.  A match is unique if it is the only
possibility or when USE-WORDS the only possibility with the same
number of words.  The shortest string of multiple possiblities will be
the best match."
  (or (if (null (cdr choices)) (cons (car choices) t))
      (let* ((regexp (concat "[^" delimiters "]*[" delimiters "]"))
	     (words (if use-words (completer-words regexp string)))
	     (choice choices)
	     (unique-p nil)
	     (match nil)
	     (match-count nil)
	     (match-len 1000))
	(while choice
	  (let* ((current (car choice))
		 (length (length current)))
	    (if match-count
		(if (= (completer-words regexp current words) words)
		    (progn
		      (setq unique-p nil)
		      (if (< length match-len)
			  (setq match current
				match-len length))))
		(if (and use-words 
			 (= (completer-words regexp current words) words))
		    (setq match current
			  match-len length
			  match-count t
			  unique-p t)
		    (if (< length match-len)
			(setq match current
			      match-len length)))))
	  (setq choice (cdr choice)))
	(cons match unique-p))))

;;;%Completer
;;;%%Utilities
(defun completer-region (delimiters)
  "Return the completion region bounded by characters in DELIMITERS
for the current buffer assuming that point is in it."
  (cons (save-excursion (skip-chars-backward delimiters) (point))
	(save-excursion (skip-chars-forward delimiters) (point))))
	 
;;;
(defun completer-last-component (string)
  "Return the start of the last filename component in STRING."
  (let ((last (1- (length string)) )
	(match 0)
	(end 0))
    (while (and (setq match (string-match "/" string end)) (< match last))
      (setq end (1+ match)))
    end))

;;;
(defun completer-match-record (string matches delimiters dir mode)
  "Return (match lcs choices unique) for STRING in MATCHES with
DELIMITERS and DIR if a filename when in MODE."
  (if (cdr matches)
      (let* ((reducing t) match lcs 
	     (dirs (concat dir string)))
	(while reducing 
	  (setq match
		(if (not completer-exact)
		    (completer-match 
		     (if dir 
			 (substring string (completer-last-component string))
			 string)
		     matches delimiters
		     completer-use-words))
		lcs (concat dir (try-completion "" (mapcar 'list matches))))
	  (if (and 
	       (not (eq mode 'help))
	       (eq (string-match lcs dirs) 0)
	       (let ((end (match-end 0)))
		 (eq (string-match (concat "[" delimiters "]") dirs end)
		     end)))
	      ;; If match up to boundary, remove other possibilities
	      (let* ((pos (match-beginning 0))
		     (regexp (concat "^" 
				     (substring lcs (length dir))
				     (regexp-quote
				      (substring dirs pos (1+ pos)))))
		     (choicep matches)
		     (new nil))
		(while choicep
		  (if (string-match regexp (car choicep))
		      (setq new (cons (car choicep) new)))
		  (setq choicep (cdr choicep)))
		(if new
		    (setq matches new)
		    (setq reducing nil)))
	      (setq reducing nil)))
	(list (if match (concat dir (car match)))
	      lcs
	      matches (if match (cdr match))))
      (if matches 
	  (let ((match (concat dir (car matches))))
	    (list match match matches t))
	  (list nil nil nil nil))))

;;;%%Complete file
(defun completer-extension-regexp (extensions)
  "Return a regexp that matches any of EXTENSIONS."
  (let ((regexp "\\("))
    (while extensions
      (setq regexp (concat regexp (car extensions)
			   (if (cdr extensions) "\\|"))
	    extensions (cdr extensions)))
    (concat regexp "\\)$")))

;;;
(defun completer-file (string pred words mode)
  "Return (match common-substring matches unique-p) for STRING using
read-file-name-internal for choices that pass PRED using WORDS to
delimit words.  If MODE is nil or 'help then possible matches will
always be returned."
  (let* ((case-fold-search completion-ignore-case)
	 (last (and (eq mode 'exit-ok) (completer-last-component string)))
	 (position
	  ;; Special hack for CMU RFS filenames
	  (if (or (string-match "^/\\.\\./[^/]*/" string)
		  ;; Special hack for ange-ftp filenames.
		  (string-match "^/[^/]*:/" string))
	      (match-end 0)
	      ;; (string-match "[^/]*$" string) to complete last only
	      (string-match "[^~/]" string)))
	 (new (substring string 0 position))
	 (user (if (string= new "~")
		   (setq new (file-name-directory
			      (expand-file-name new)))))
	 (words (concat words "/"))
	 (len (length string))
	 (choices nil)
	 (old-choices (list nil nil nil nil)))
    (while position
      (let* ((begin (string-match "/" string position))
	     (end (if begin (match-end 0)))
	     (exact-p nil))
	(setq choices
	      ;; Ends with a /, so check files in directory
	      (if (and (memq mode '(nil help)) (= position len))
		  (completer-match-record 
		   ""
		   (all-completions new 'read-file-name-internal pred)
		   words new mode)
		  (if (eq position last)
		      (let ((new (concat new (substring string position))))
			(list new new nil t))
		      (completer
		       (concat new (substring string position end))
		       'read-file-name-internal pred words mode t))))
	;; Keep going if unique or we match exactly
	(if (or (car (cdr (cdr (cdr choices))))
		(setq exact-p
		      (string= (concat new (substring string position end))
			       (car choices))))
	    (setq old-choices
		  (let* ((lcs (car (cdr choices)))
			 (matches (car (cdr (cdr choices))))
			 (slash (and lcs (string-match "/$" lcs))))
		    (list nil
			  (if slash (substring lcs 0 slash) lcs)
			  (if (and (cdr matches) 
				   (or (eq mode 'help) (not exact-p)))
			      matches)
			  nil))
		  new (car choices)
		  position end)
	    ;; Its ok to not match user names because they may be in
	    ;; different root directories
	    (if (and (= position 1) (= (elt string 0) ?~))
		(setq new (substring string 0 end)
		      choices (list new new (list new) t)
		      user nil
		      position end)
		(setq position nil)))))
    (if (not (car choices))
	(setq choices old-choices))
    (if (and (car choices)
	     (not (eq mode 'help))
	     (not (car (cdr (cdr (cdr choices))))))
	;; Try removing completion ignored extensions
	(let ((new-choices nil)
	      (choicep (car (cdr (cdr choices))))
	      (extensions
	       (completer-extension-regexp
		completion-ignored-extensions))
	      (removed nil))
	  (while choicep
	    (let ((choice (car choicep)))
	      (if (string-match extensions choice)
		  (setq removed t)
		  (setq new-choices (cons choice new-choices))))
	    (setq choicep (cdr choicep)))
	  (if (and removed new-choices)
	      (setq choices
		    (completer-match-record 
		     string new-choices words
		     (file-name-directory (car choices))
		     mode)))))
    (if user
	(let ((match (car choices))
	      (lcs (car (cdr choices)))
	      (len (length user)))
	  (setq choices
		(cons (if match (concat "~" (substring match len)))
		      (cons (if lcs (concat "~" (substring lcs len)))
			    (cdr (cdr choices)))))))
    choices))

;;;%Exported program interface
;;;%%Completer
(defun completer (string table pred words
			 &optional mode file-p)
  "Return (match common-substring matches unique-p) for STRING in
TABLE for choices that pass PRED using WORDS to delimit words.  If the
flag completer-complete-filenames is T and the table is
read-file-name-internal, then filename components will be individually
expanded.  Optional MODE is nil for complete, 'help for help and 'exit
for exit."
  (if (and completer-complete-filenames
	   (not file-p) (eq table 'read-file-name-internal))
      (completer-file string pred words mode)
      (let* ((case-fold-search completion-ignore-case)
	     (pattern (concat "[" words "]"))
	     (component (if file-p (completer-last-component string)))
	     (dir (if component (substring string 0 component)))
	     (string (if dir (substring string component) string))
	     (has-words (string-match pattern string)))
	(if (and file-p (string-match "^\\$" string))
	    ;; Handle environment variables
	    (let ((match
		   (getenv (substring string 1 
				      (string-match "/" string)))))
	      (if match (setq match (concat match "/")))
	      (list match match (list match) match))
	    (if (or has-words file-p)
		(let* ((choices
			(all-completions 
			 (concat dir (substring string 0 has-words))
			 table pred))
		       (regexp (completer-regexp string words))
		       (choicep choices)
		       (matches nil))
		  (while choicep
		    (let ((choice (car choicep)))
		      (if (string-match regexp choice)
			  (setq matches (cons choice matches)))
		      (setq choicep (cdr choicep))))
		  (completer-match-record string matches words dir mode))
		(list nil nil nil nil))))))

;;;%%Display choices
(defun completer-display-choices (choices &optional match message end
					  display)
  "Display the list of possible CHOICES with optional MATCH, MESSAGE,
END and DISPLAY.  If MATCH is non-nil, it will be flagged as the best
guess.  If there are no choices, display MESSAGE.  END is where to put
temporary messages.  If DISPLAY is present then it will be called on
each possible completion and should return a string."
  (if choices
      (with-output-to-temp-buffer " *Completions*"
	(if (cdr choices) 
	    (display-completion-list
	     (if display
		 (let ((new))
		   (while choices
		     (setq new (cons (funcall display (car choices)) new)
			   choices (cdr choices)))
		   (setq choices new))
		 choices)))
	(if match
	    (save-excursion
	      (set-buffer " *Completions*")
	      (goto-char (point-min))
	      (insert "Guess = " match (if (cdr choices) ", " "")))))
      (beep)
      (completer-message (or message " (No completions)") end)))

;;;%%Goto
(defun completer-goto (match lcs choices unique delimiters words 
			     &optional mode display)
  "MATCH is the best match, LCS is the longest common substring of all
of the matches.  CHOICES is a list of the possibilities, UNIQUE
indicates if MATCH is unique.  DELIMITERS are possible bounding
characters for the completion region.  WORDS are the characters that
delimit the words for partial matches.  Replace the region bounded by
delimiters with the match if unique and the lcs otherwise unless
optional MODE is 'help.  Then go to the part of the string that
disambiguates choices using WORDS to separate words and display the
possibilities if the string was not extended.  If optional DISPLAY is
present then it will be called on each possible completion and should
return a string."
  (setq completer-message nil)
  (let* ((region (completer-region delimiters))
	 (start (car region))
	 (end (cdr region))
	 (string (buffer-substring start end))
	 (file-p (string-match "^~\\|^/\\|^$" string))
	 (no-insert (eq mode 'help))
	 (message t)
	 (new (not (string= (buffer-substring start (point)) lcs))))
    (if unique
	(if no-insert
	    (progn
	      (goto-char end)
	      (completer-display-choices choices match nil end display))
	    (if (string= string match)
		(if (not file-p) 
		    (progn (goto-char end)
			   (completer-message " (Sole completion)" end)))
		(completer-insert match delimiters)))
	(if lcs
	    (let* ((regexp 
		    (concat "[" words (if file-p "/") "]"))
		   (words (completer-words regexp lcs))
		   point)
	      ;; Go to where its ambiguous
	      (goto-char start)
	      (if (not no-insert)
		  (progn 
		    (insert lcs)
		    (setq completer-last-pattern 
			  (list string delimiters (current-buffer) start)
			  start (point)
			  end (+ end (length lcs)))))
	      ;; Skip to the first delimiter in the original string
	      ;; beyond the ambiguous point and keep from there on
	      (if (re-search-forward regexp end 'move words)
		  (forward-char -1))
	      (if (not no-insert) 
		  (progn
		    (setq end (- end (- (point) start)))
		    (delete-region start (point))))))
	(if choices
	    (if (or no-insert (not new))
		(completer-display-choices choices match nil end display))
	    (if file-p 
		(progn 
		  (if (not (= (point) end)) (forward-char 1))
		  (if (not (save-excursion (re-search-forward "/" end t)))
		      (progn (goto-char end)
			     (if (eq mode 'exit-ok)
				 (setq message nil))))))
	    (if message
		(progn
		  (beep)
		  (completer-message (if no-insert 
					 " (No completions)"
					 " (No match)")
				     end)))))))	    

;;;%Exported buffer interface
;;;%%Complete and go
(defun completer-complete-goto (delimiters words table pred 
					   &optional no-insert display)
  "Complete the string bound by DELIMITERS using WORDS to bound words
for partial matches in TABLE with PRED and then insert the longest
common substring unless optional NO-INSERT and go to the point of
ambiguity.  If optional DISPLAY, it will be called on each match when
possible completions are shown and should return a string."
  (let* ((region (completer-region delimiters)))
    (apply 'completer-goto 
	   (nconc (completer (buffer-substring (car region) (cdr region))
			     table pred words no-insert)
		  (list delimiters words no-insert display)))))

;;;%%Undo
(defun completer-insert (match delimiters &optional buffer undo)
  "Replace the region bounded with characters in DELIMITERS by MATCH
and save it so that it can be restored by completer-undo."
  (let* ((region (completer-region delimiters))
	 (start (car region))
	 (end (cdr region)))
    (if (and undo (or (not (= start undo)) 
		      (not (eq (current-buffer) buffer))))
	(error "No previous pattern")
	(setq completer-last-pattern (list (buffer-substring start end) 
					   delimiters
					   (current-buffer)
					   start))
	(delete-region start end)
	(goto-char start)
	(insert match))))

;;;
(defun completer-undo ()
  "Swap the last expansion and the last match pattern."
  (interactive)
  (if completer-last-pattern
      (apply 'completer-insert completer-last-pattern)
      (error "No previous pattern")))

;;;%Minibuffer specific code
;;;%%Utilities
(defun completer-minibuf-string ()
  "Remove dead filename specs from the minibuffer as delimited by //
or ~ or $ and return the resulting string."
  (save-excursion
    (goto-char (point-max))
    (if (and (eq minibuffer-completion-table 'read-file-name-internal)
	     (re-search-backward "//\\|.~.\\|.\\$" nil t))
	(delete-region (point-min) (1+ (point))))
    (buffer-substring (point-min) (point-max))))

;;;
(defun completer-minibuf-exit ()
  "Exit and clear pattern."
  (interactive)
  (setq completer-last-pattern nil)
  (exit-minibuffer))

;;;
(defun completer-new-cmd (cmd)
  "Return T if we can't execute the old minibuffer version of CMD."
  (if (or completer-disable
	  (let ((string (completer-minibuf-string)))
	    (or
	     (not (string-match
		   (concat "[" completer-words "/~]")
		   string))
	      (condition-case ()
		  (let ((completion
			 (try-completion string
					 minibuffer-completion-table
					 minibuffer-completion-predicate)))
		    (if (eq minibuffer-completion-table
			    'read-file-name-internal)
			;; Directories complete as themselves
			(and completion
			     (or (not (string= string completion))
				 (file-exists-p completion)))
			completion))
		(error nil)))))
      (progn
	(funcall cmd)
	nil)
      t))

;;;
(defun completer-minibuf (&optional mode)
  "Partial completion of minibuffer expressions.  Optional MODE is
'help for help and 'exit for exit.

If what has been typed so far matches any possibility normal
completion will be done.  Otherwise, the string is considered to be a
pattern with words delimited by the characters in
completer-words.  If completer-exact is T, the best match will be
the shortest one with the same number of words as the pattern if
possible and otherwise the shortest matching expression.  

Examples:
a-f     auto-fill-mode
r-e     rmail-expunge
b--d    *begining-of-defun or byte-recompile-directory
~/i.e   *~/ilisp.el or ~/il-el.el or ~/ilisp.elc
/u/mi/  /usr/misc/"
  (interactive)
  (nconc
   (completer (completer-minibuf-string)
	      minibuffer-completion-table
	      minibuffer-completion-predicate
	      completer-words
	      mode)
   (list "^" completer-words mode)))

;;;%%Commands
(defun completer-toggle ()
  "Turn partial completion on or off."
  (interactive)
  (setq completer-disable (not completer-disable))
  (message (if completer-disable 
	       "Partial completion OFF"
	       "Partial completion ON")))

;;;
(defvar completer-old-help
  (lookup-key minibuffer-local-must-match-map "?")
  "Old binding of ? in minibuffer completion map.")
(defun completer-help ()
  "Partial completion minibuffer-completion-help.  
See completer-minibuf for more information."
  (interactive)
  (if (completer-new-cmd completer-old-help)
      (apply 'completer-goto (completer-minibuf 'help))))

;;;
(defvar completer-old-completer
  (lookup-key minibuffer-local-must-match-map "\t")
  "Old binding of TAB in minibuffer completion map.")
(defun completer-complete ()
  "Partial completion minibuffer-complete.
See completer-minibuf for more information."
  (interactive)
  (if (completer-new-cmd completer-old-completer)
      (apply 'completer-goto (completer-minibuf))))

;;;
(defvar completer-old-word
  (lookup-key minibuffer-local-must-match-map " ")
  "Old binding of SPACE in minibuffer completion map.")
(defun completer-word ()
  "Partial completion minibuffer-complete.
See completer-minibuf for more information."
  (interactive)
  (if (completer-new-cmd completer-old-word)
      (apply 'completer-goto (completer-minibuf))))

;;; 
(defvar completer-old-exit
  (lookup-key minibuffer-local-must-match-map "\n")
  "Old binding of RET in minibuffer completion map.")
(defun completer-exit (&optional short)
  "Partial completion minibuffer-complete-and-exit.
See completer-minibuf for more information."
  (interactive)
  (if (or short (completer-new-cmd completer-old-exit))
      (let* ((completions (completer-minibuf 'exit))
	     (match (car completions))
	     (unique-p (car (cdr (cdr (cdr completions))))))
	(apply 'completer-goto completions)
	(if unique-p 
	    (completer-minibuf-exit)
	    (if (and match short)
		(progn (completer-insert match "^")
		       (if minibuffer-completion-confirm
			   (completer-message " (Confirm)")
			   (completer-minibuf-exit)))
		(if (not completer-message) (beep)))))))

;;;
(defun completer-match-exit ()
  "Exit the minibuffer with the current best match."
  (interactive)
  (completer-exit t))

;;;
(defun completer-local-exit ()
  "Partial completion exit-minibuffer.
See completer-minibuf for more information."
  (interactive)
  (let* ((string (completer-minibuf-string))
	 (start (completer-last-component string)))
    ;; Complete filenames except for last component.
    (if start (apply 'completer-goto (completer-minibuf 'exit-ok)))
    (completer-minibuf-exit)))

;;;%%Keymaps
(define-key minibuffer-local-completion-map "\C-_"  'completer-undo)
(define-key minibuffer-local-completion-map "\t"    'completer-complete)
(define-key minibuffer-local-completion-map " "     'completer-word)
(define-key minibuffer-local-completion-map "?"     'completer-help)
(define-key minibuffer-local-completion-map "\n"    'completer-minibuf-exit)
(define-key minibuffer-local-completion-map "\r"    'completer-minibuf-exit)
(define-key minibuffer-local-completion-map "\M-\n" 'completer-local-exit)
(define-key minibuffer-local-completion-map "\M-\r" 'completer-local-exit)

(define-key minibuffer-local-must-match-map "\C-_"  'completer-undo)
(define-key minibuffer-local-must-match-map "\t"    'completer-complete)
(define-key minibuffer-local-must-match-map " "     'completer-word)
(define-key minibuffer-local-must-match-map "\n"    'completer-exit)
(define-key minibuffer-local-must-match-map "\r"    'completer-exit)
(define-key minibuffer-local-must-match-map "?"     'completer-help)
(define-key minibuffer-local-must-match-map "\M-\n" 'completer-match-exit)
(define-key minibuffer-local-must-match-map "\M-\r" 'completer-match-exit)

;;;%comint 
(defun completer-comint-dynamic-list-completions (prefix)
  "Display the list of possible file name completions.  With a
negative prefix, undo the last completion."
  (interactive "P")
  (completer-comint-dynamic-complete prefix 'help))

;;;
(defun completer-comint-dynamic-complete (&optional undo mode)
  "Complete the previous filename or display possibilities if done
twice in a row.  If called with a prefix, undo the last completion."
  (interactive "P")
  (if undo
      (completer-undo)
      (completer-complete-goto 
       "^ \t\n\""
       completer-words
       'read-file-name-internal
       default-directory
       mode)))
(fset 'comint-dynamic-complete 'completer-comint-dynamic-complete)
(fset 'comint-dynamic-list-completions 
      'completer-comint-dynamic-list-completions)

;;; Set the functions again if comint is loaded
(setq comint-load-hook 
      (cons (function (lambda ()
	      (fset 'comint-dynamic-complete 
		    'completer-comint-dynamic-complete)
	      (fset 'comint-dynamic-list-completions 
		    'completer-comint-dynamic-list-completions)))
	    (if (and (boundp 'comint-load-hook) comint-load-hook)
		(if (consp comint-load-hook) 
		    (if (eq (car comint-load-hook) 'lambda)
			(list comint-load-hook)
			comint-load-hook)
		    (list comint-load-hook)))))

;;;%lisp-complete-symbol
(defun lisp-complete-symbol (&optional mode)
  "Perform partial completion on Lisp symbol preceding point.  That
symbol is compared against the symbols that exist and any additional
characters determined by what is there are inserted.  If the symbol
starts just after an open-parenthesis, only symbols with function
definitions are considered.  Otherwise, all symbols with function
definitions, values or properties are considered.  If called with a
negative prefix, the last completion will be undone."
  (interactive "P")
  (if (< (prefix-numeric-value mode) 0)
      (completer-undo)
      (let* ((end (save-excursion (skip-chars-forward "^ \t\n)]}\"") (point)))
	     (beg (save-excursion
		    (backward-sexp 1)
		    (while (= (char-syntax (following-char)) ?\')
		      (forward-char 1))
		    (point)))
	     (pattern (buffer-substring beg end))
	     (predicate
	      (if (eq (char-after (1- beg)) ?\()
		  'fboundp
		  (function (lambda (sym)
		    (or (boundp sym) (fboundp sym)
			(symbol-plist sym))))))
	     (completion (try-completion pattern obarray predicate)))
	   (cond ((eq completion t))
	      ((null completion)
	       (completer-complete-goto
		"^ \t\n\(\)[]{}'`" completer-words
		obarray predicate 
		nil
		(if (not (eq predicate 'fboundp))
		    (function (lambda (choice)
		      (if (fboundp (intern choice))
			  (list choice " <f>")
			  choice))))))
	      ((not (string= pattern completion))
	       (delete-region beg end)
	       (insert completion))
	      (t
	       (message "Making completion list...")
	       (let ((list (all-completions pattern obarray predicate)))
		 (or (eq predicate 'fboundp)
		     (let (new)
		       (while list
			 (setq new (cons (if (fboundp (intern (car list)))
					     (list (car list) " <f>")
					     (car list))
					 new))
			 (setq list (cdr list)))
		       (setq list (nreverse new))))
		 (with-output-to-temp-buffer "*Help*"
		   (display-completion-list list)))
	       (message "Making completion list...%s" "done"))))))

;;;%Hooks
(provide 'completer)
(run-hooks 'completer-load-hook)

