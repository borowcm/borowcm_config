;; Load CPerl mode
(setq auto-mode-alist
      (append '(("\\.[pP][Llm]$" . cperl-mode))  auto-mode-alist ))
(setq interpreter-mode-alist (append interpreter-mode-alist
 				        '(("miniperl" . cperl-mode))))
(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(setq cperl-hairy t)
(setq cperl-indent-level 2)
(setq cperl-electric-keywords t)

(defun insert-perl-file-header ()
  "Inserts a perl-style header"
  (interactive)
  (progn
    (insert-string (concat "eval '(exit $?0)' && eval 'exec perl -w -S $0 ${1+\"$@\"}' && eval 'exec perl -w -S $0 $argv:q'  if 0;" )) 
    (newline)
    (insert-string (concat "# Filename    : " (buffer-name))) (newline)
    (insert-string (concat "# Author      : " (user-full-name))) (newline)
    (insert-string (concat "# Written on  : " (current-time-string))) (newline)
    (insert-string "# Purpose     : ") (newline)
    (insert-string "#               ") (newline)
    (insert-string "#               ") (newline)
    (insert-string "# Comments    : ") (newline)
    (insert-string "#               ") (newline) (newline)
    (insert-string "use strict;      # comment out these two") (newline)
    (insert-string "use diagnostics; # and '-w' switch, once testing is over ") (newline)
    (save-buffer))
)

(define-key global-map "\C-cP" 'insert-perl-file-header)
