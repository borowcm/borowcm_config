(defun line-to-top-of-window ()
  "Move the line point is on to top of window."
  (interactive) 
  (recenter 0)
)

;; I always like to open ssh shell to my "compile machine" : papaya 
;; and using this hot-key function I can "switch" to it easily.
(defun my-switch-to-papaya() ;; mapped to 'C-c p'
  (interactive)
  (ssh "papaya.ececs.uc.edu")
)

(defun my-switch-to-olympus() ;; mapped to 'C-c C-o'
  (interactive)
  (ssh "olympus.ececs.uc.edu")
)

(condition-case ()
    (quietly-read-abbrev-file)
  (file-error nil))

(defun my-quietly-read-abbrev-file()
  (interactive)
  (setq abbrev-mode t)
  (quietly-read-abbrev-file "~/.abbrev_defs"))  

;; Load stuff explicitly for XEmacs/Lucid Emacs
(when (or running-xemacs211 )
  (defun my-toggle-menubar()
    (interactive)
    (set-specifier menubar-visible-p
		   (not (specifier-instance menubar-visible-p)))
    )
  
  (defun my-toggle-toolbar()
    (interactive)
    (set-specifier right-toolbar-visible-p
		   (not ( specifier-instance right-toolbar-visible-p)))
    )
)

(defun insert-vhdl-file-header()
  "Inserts header for a vhdl file"
  (interactive)
  (progn
    (insert-string (concat "************************************************"))
    (insert-string (concat "*************")) (newline)
    (insert-string (concat "-- Filename    : " (buffer-name))) (newline)
    (insert-string (concat "-- Author      : " (user-full-name))) (newline)
    (insert-string (concat "-- Written on  : " (current-time-string))) (newline)
    (insert-string "-- Purpose     : ") (newline)
    (insert-string "--               ") (newline)
    (insert-string "--               ") (newline)
    (insert-string "-- Comments    : ") (newline)
    (insert-string "--               ") (newline)
    (insert-string (concat "************************************************"))
    (insert-string (concat "*************")) (newline)
    (save-buffer))
)

(defun go-matching-paren ()
  "Move cursor to the matching parenthesis."
  (interactive)
  (cond ((looking-at "[[({]") (forward-sexp 1) (backward-char 1))
        ((looking-at "[])}]") (forward-char 1) (backward-sexp 1))
        (t (ding) (message "Unbalanced parenthesis"))))

(defun show-matching-paren ()
  "Move cursor momentarily to the matching parenthesis."
  (interactive)
  (save-excursion
    (cond ((looking-at "[[({]") (forward-sexp 1) (backward-char 1) (sit-for
1))
          ((looking-at "[])}]") (forward-char 1) (backward-sexp 1) (sit-for 1))
          (t (ding) (message "Unbalanced parenthesis")))))

