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

