(require 'cl)
(defun* count-words-region (begin end &optional
				     (interactivep (interactive-p))
				     (region-name "region"))
  "Count words in the (interactive: active) region."
  (interactive "r")
  (save-excursion
    (setf (point) begin)
    (let ((words
           (loop while (re-search-forward "\\w+\\W*" end t)
                 sum 1)))
      (when interactivep
        (cond ((zerop words)
               (message
                "The %s does NOT have any words." region-name))
	      ((= 1 words)
	       (message "The %s has 1 word." region-name))
	      (t
	       (message
		"The %s has %d words." region-name words))))
      words)))

(defun count-words-buffer ()
  "Count words in the current buffer."
  (interactive)
  (count-words-region (point-min) (point-max)
		      (interactive-p) "buffer"))
(provide 'count-words)
