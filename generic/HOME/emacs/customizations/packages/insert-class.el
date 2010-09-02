(defun my-make-class-def (class parent)
  "Insert a string \"class CLASS :public(PARENT)\" into the buffer,
   the CLASS and PARENT are prompted for in the minibuffer."
  (interactive "sClass: \nsParent: ")
  (let ((class-header (format "\nclass %s :public(%s)  {\n" class parent)))
    (insert class-header)))
