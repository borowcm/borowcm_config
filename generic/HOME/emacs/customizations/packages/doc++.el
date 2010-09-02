;; DOC++ commands

(defun insert-doc++-comment ()
  "Inserts a multi-line doc++ comment /** .. */"
  (interactive)
   (progn
   (insert-string "/**      */")))

(defun insert-doc++-block ()
  "Inserts a doc++ block //@{ ...  //@}"
  (interactive)
   (progn
   (insert-string "//@{       //@}")))

