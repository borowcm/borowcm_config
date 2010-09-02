;;; basis-utils.el --- miscellaneous utilities

;; Copyright (C) 1999-2000 Basis Technology, Corp.

;; Author: Tom Emerson <tree@basistech.com>
;; Created: Sometime in Fall '99 
;; Keywords: basis utilities source code

;;; Commentary:

;; This package contains a random collection of functions I've written
;; to automate some of the repetitive editing tasks when writing new
;; code. A side benefit is that they implicitly enforce our coding
;; conventions.

;; Last modified: 2000-07-09

;;; Code:

(require 'doxygen)

(defconst bt-include-directive-regexp
  "^[ \t]*#include[ \t]+[<\"]\\([a-zA-Z0-9_.]+\.h\\)[>\"][ \t]*$"
  "Regular expression used to detect #include directives")

; FIXME: this should really get the comment character from the current
;        mode so that it can be used for other languages too.
(defun basis-insert-std-copyright ()
  "Insert the standard Basis copyright notice at the current point."
  (interactive "*")
  (insert
   (format
    (concat
     "/******************************************************************************\n"
     "** This data and information is proprietary to, and a valuable trade secret\n"
     "** off, Basis Technology Corp.  It is given in confidence by Basis Technology\n"
     "** and may only be used as permitted under the license agreement under which\n"
     "** it has been distributed, and in no other way.\n"
     "**\n"
     "** Copyright (c) %s Basis Technology Corp. All rights reserved.\n"
     "**\n"
     "** The technical data and information provided herein are provided with\n"
     "** `limited rights', and the computer software provided herein is provided\n"
     "** with `restricted rights' as those terms are defined in DAR and ASPR\n"
     "** 7-104.9(a).\n"
     "******************************************************************************/\n\n")
    (format-time-string "%Y")))
  (doxygen-insert-file-comment))

;;; the following were added 2000-07-09

(defun basis-insert-include-file (filename)
  "Insert a guarded #include directive for the specified filename."
  (interactive "FInclude file name: ")
  (let ((fname (file-name-nondirectory filename)))
    (bt-insert-guard-ifdef fname)
    (insert (format (concat "#include \"%s\"\n"
                            "#endif\n")
                    fname))))

(defun basis-insert-include-file-guards ()
  "Wrap the current #include directive with the correct guard macros."
  (interactive "*")
  (beginning-of-line)
  (when (re-search-forward bt-include-directive-regexp nil t)
    (forward-line 0)
    (bt-insert-guard-ifdef (match-string 1))
    (forward-line 1)
    (insert "#endif\n")))

; FIXME: this function does not skip already guarded directives
(defun basis-insert-include-file-guards-in-region (start end)
  "Insert include file guards around all unguarded #include directives in
the supplied region."
  (interactive "r")
  (goto-char start)
  (forward-line -1)
  ; need to use a marker to track the end of the region, since we'll
  ; be inserting text as we walk through it.
  (let ((end-marker (set-marker (make-marker) end)))
    (while (re-search-forward bt-include-directive-regexp
                              (marker-position end-marker)
                              t)
      (forward-line 0)
      (insert-include-file-guards)
      (forward-line 2))))

;; Private utility functions

(defun bt-insert-guard-ifdef (filename)
  (insert (format "#ifndef %s\n"
          (bt-guard-macro-from-filename filename))))

(defun bt-guard-macro-from-filename (file)
  "Generate the guard macro for the supplied filename."
  (let ((guard (format "__%s__" (upcase file))))
    (while (string-match "\\." guard)
      (aset guard (match-beginning 0) ?_))
    guard))


(provide 'basis-utils)

;;; basis-utils.el ends here
