;;; color-theme.el --- install color themes

;; Copyright (C) 1999, 2000  Jonadab the Unsightly One <jonadab@bright.net>

;; Author: Jonadab the Unsightly One <jonadab@bright.net>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 4.3.4
;; Keywords: faces

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Sharing your current color setup:

;; If you have already invested time in customizing Emacs faces, please
;; consider sharing your current setup.  Make sure that color-theme.el
;; is in your `load-path'.  Type M-x load-library RET color-theme RET to
;; load all the functions.  Type M-x color-theme-print RET and mail the
;; result to the maintainer of this package (see above for mail addres).

;; If you want to make sure that all your customization was exported,
;; type M-x list-faces-display RET to get a list of all faces currently
;; defined.  This is the list of faces that `color-theme-print' uses.

;; Installing a color theme:

;; Make sure that color-theme.el is in your `load-path'.  Type M-x
;; load-library RET color-theme RET to load all the functions.

;; The main function to call is color-theme-select.  Type M-x
;; color-theme-select RET.  That creates a Color Theme Selection
;; buffer.  Press RET or `i' on a color theme to install it for the
;; rest of your session.

;; If you want to install the color theme as soon as Emacs is started
;; up, read the description of the theme you like and remember the
;; name of the color theme function.  Press `d' on a color theme in
;; the Color Theme Selection buffer to read the description.  Assuming
;; you like the Gnome2 theme, you'll find that the function to use is
;; called `color-theme-gnome2'.  Add the following to the end of your
;; .emacs (removing the leading `;;').

;; (require 'color-theme)
;; (color-theme-gnome2)

;; Deriving your own color theme:

;; If you want to derive your own color theme from an existing color
;; theme, press `p' in the Color Theme Selection buffer (it doesn't
;; matter where in the buffer you press `p'.  This creates a buffer with
;; the elisp code needed to install the current color theme.  Copy the
;; entire code to your .emacs and start fooling around with it.  Read
;; the documentation of color-theme-install using C-h f
;; color-theme-install RET.

;; Note that all color themes are cumulative.  You can try to combine
;; several color themes.  This makes sense if one color theme defines
;; faces which another color theme does not.  Install both themes by
;; pressing RET or `i' on them in the Color Theme Selection buffer,
;; press `p' to get the elisp code, paste it into your .emacs and
;; start working on your masterpiece.

;; If your color theme is but a variation of an existing color theme,
;; install the parent color theme, make the modifications you want,
;; and then use C-u p or C-u M-x color-theme-print to avoid
;; duplicating settings from the parent color theme.

;; Changing menu colors:

;; If are using X, you can set the menu foreground and background using
;; your .Xdefaults file.  If you set emacs*Background and
;; emacs*Foreground, the first frame will be created with these
;; foreground and background colors used for the menu.  If your .emacs
;; loads a color theme, the frame foreground and background colors
;; overwrite the settings from the .Xdefaults file in the frame itself,
;; but not for the menu.  This assumes that you are not setting any menu
;; ressources for Emacs in the .Xdefaults file.  Here is a sample entry
;; for your .Xdefaults file:
;;   emacs*Background:		DarkSlateGray
;;   emacs*Foreground:		wheat

;;; Thanks

;; S. Pokrovsky <pok@nbsp.nsk.su> for ideas and discussion.
;; All the users that contributed their color themes.

;;; Bugs:

;; Probably XEmacs incompatible; contributions welcome.
;; Not compatible with the forthcoming themes code for Emacs 21.



;;; Code:

(defvar color-theme-legal-frame-parameters "\\(color\\|mode\\)$"
  "Regexp that matches frame parameter names.
Only frame parameter names that match this regexp can be changed as part
of a color theme.")

(defvar color-theme-legal-variables "face$"
  "Regexp that matches variable names.
Only variables that match this regexp can be changed as part of a color
theme.  In addition to matching this name, the variables have to be user
variables (see function `user-variable-p').")

(defvar color-theme-illegal-faces "^w3-"
  "Regexp that matches face names forbidden in themes.
The default setting \"^w3-\" excludes w3 faces since these
are created dynamically, anyway.")

(defvar color-theme-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'color-theme-install-at-point)
    (define-key map (kbd "c") 'list-color-display)
    (define-key map (kbd "d") 'color-theme-describe)
    (define-key map (kbd "f") 'list-faces-display)
    (define-key map (kbd "i") 'color-theme-install-at-point)
    (define-key map (kbd "p") 'color-theme-print)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "?") 'color-theme-describe)
    (define-key map [mouse-2] 'color-theme-install-at-mouse)
    (define-key map [S-mouse-2] 'color-theme-install-at-mouse)
    map)
  "Mode map used for the buffer created by `color-theme-select'.")

(defvar color-theme-buffer-name "*Color Theme Selection*"
  "Name of the color theme selection buffer.")

;;; List of color themes used to create the *Color Theme Selection*
;;; buffer.

(defvar color-themes
  '((color-theme-billw "Billw" "Bill White <billw@wolfram.com>")
    (color-theme-simple-1 "Black" "Jonadab <jonadab@bright.net>")
    (color-theme-fischmeister "Fischmeister" 
			      "Sebastian Fischmeister <sfischme@nexus.lzk.tuwien.ac.at>")
    (color-theme-gnome "Gnome" "Jonadab <jonadab@bright.net>")
    (color-theme-gnome2 "Gnome 2" "Alex <a.schroeder@bsiag.ch>")
    (color-theme-jonadabian "Jonadab" "Jonadab <jonadab@bright.net>")
    (color-theme-retro-green "Retro Green" "Alex <a.schroeder@bsiag.ch>")
    (color-theme-retro-orange "Retro Orange" "Alex <a.schroeder@bsiag.ch>")
    (color-theme-ryerson "Ryerson" "Luis Fernandes <elf@ee.ryerson.ca>")
    (color-theme-sitaramv-nt "Sitaram NT"
			     "Sitaram Venkatraman <sitaramv@loc251.tandem.com>")
    (color-theme-sitaramv-solaris "Sitaram Solaris"
				  "Sitaram Venkatraman <sitaramv@loc251.tandem.com>")
    (color-theme-standard "Standard" "Emacs Team, added by Alex <a.schroeder@bsiag.ch>")
    (color-theme-subtle-hacker "Subtle Hacker" "Colin Walters <levanti@verbum.org>")
    (color-theme-wheat "Wheat" "Alex <a.schroeder@bsiag.ch>")
    (color-theme-pok-wob "White On Black" "S. Pokrovsky <pok@nbsp.nsk.su>")
    (color-theme-pok-wog "White On Grey" "S. Pokrovsky <pok@nbsp.nsk.su>"))
  "List of color themes.  

Each THEME is itself a three element list (FUNC NAME MAINTAINER).  

FUNC is a color theme function which does the setup.  The function
FUNC may call `color-theme-install'.  The color theme function may be
interactive.

NAME is the name of the theme and MAINTAINER is the name and/or email of
the maintainer of the theme.

If you defined your own color theme and want to add it to this list,
use this:

  (add-to-list 'color-themes '(color-theme-gnome2 \"Gnome2\" \"Alex\"))")

;;; Functions

(defun color-theme-select ()
  "Displays a special buffer for selecting and installing a color theme."
  (interactive)
  (switch-to-buffer (get-buffer-create color-theme-buffer-name))
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((themes color-themes))
    (while themes
      (let* ((theme (car themes))
	     (func (nth 0 theme))
	     (name (nth 1 theme))
	     (author (nth 2 theme))
	     (desc))
	(setq desc (format "%-23s %s" name author))
	(put-text-property 0 (length desc) 'color-theme func desc)
	(put-text-property 0 (length name) 'face 'bold desc)
	(put-text-property 0 (length name) 'mouse-face 'highlight desc)
        (insert desc)
	(newline))
      (setq themes (cdr themes))))
  (beginning-of-buffer)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (color-theme-mode))

(defun color-theme-mode ()
  "Major mode to select and install color themes.

Use \\[color-theme-install-at-point] to install a color theme: the changes are applied to all
frames.

Note that the changes are applied on top of your current setup.  This is
a feature.

Note that some of the themes should be considered extensions to the
standard color theme; they modify only a limited number of faces.  To
verify the final look of a color theme, install the standard color
theme, then install the other color theme.  This is a feature; it allows
you to mix several color themes.

If you want to install a color theme permanently, use \\[color-theme-print] 
and copy the result into your .emacs file or put the call to the color
theme function into your .emacs.  See `color-theme-print' for more information.

Note that the Emacs menu is not affected by color themes within Emacs.
Depending on the toolkit you used to compile Emacs, you might have to
set specific X ressources.  See the info manual for more information.

\\{color-theme-mode-map}

The color themes are listed in `color-themes', which see."
  (kill-all-local-variables)
  (setq major-mode 'color-theme-mode)
  (setq mode-name "Color Themes")
  (use-local-map color-theme-mode-map)
  (goto-address))

;;; Commands in Color Theme Selection mode

(defun color-theme-describe ()
  "Describe color theme listed at point.
This shows the documentation of the value of text-property color-theme
at point.  The text-property color-theme should be a color theme
function.  See `color-themes'."
  (interactive)
  (describe-function (get-text-property (point) 'color-theme)))

(defun color-theme-install-at-mouse (event)
  "Install color theme clicked upon using the mouse.
First argument EVENT is used to set point.  Then
`color-theme-install-at-point' is called."
  (interactive "e")
  (save-excursion
    (let ((posn (event-start event)))
      (set-buffer (window-buffer (posn-window posn)))
      (goto-char (posn-point posn))
      (color-theme-install-at-point))))

(defun color-theme-install-at-point ()
  "Installs color theme at point.
This calls the value of the text-property `color-theme' at point.
The text-property `color-theme' should be a color theme function.
See `color-themes'."
  (interactive)
  (let ((func (get-text-property (point) 'color-theme)))
    (if func
	(funcall func))))

;; Taking a snapshot of the current color theme and pretty printing it.

(defun color-theme-filter (old-list regexp &optional exclude)
  "Filter OLD-LIST.
The resulting list only contains elements with names matching REGEXP.
If the element is a list, then the name of the car of the list is used.
Therefore, OLD-LIST may also be an alist -- in that case the keys are
used to determine wether an element stays or goes.

If the optional argument EXCLUDE is non-nil, then the sense is
reversed: only non-matching elements will be retained."
  (let ((elem) (new-list))
    (while old-list
      (setq elem (car old-list))
      (setq name (symbol-name (if (listp elem) (car elem) elem)))
      (setq old-list (cdr old-list))
      (if (or (and (not exclude)
		   (string-match regexp name))
	      (and exclude
		   (not (string-match regexp name))))
	    (add-to-list 'new-list elem)))
    new-list))

(defun color-theme-spec (face)
  "Return a list for FACE which has the form (FACE SPEC).
See `defface' for the format of SPEC.  In this case we use only one
DISPLAY, t, and determine ATTS using `face-attr-construct'.
If ATTS is nil, (nil) is used  instead."
  (let ((atts (face-attr-construct face)))
    (if atts
	`(,face ((t ,atts)))
      `(,face ((t (nil)))))))

(defun color-theme-get-params (&optional master-params)
  "Return a list of frame parameter settings usable in a color theme.
Such an alist may be installed by `color-theme-install-frame-params'.  The
frame parameters returned must match `color-theme-legal-frame-parameters'.

If the optional argument MASTER-PARAMS is provided, then the alist returned
will only contain frame parameters with settings differing from MASTER-PARAMS."
  (let ((params (color-theme-filter (frame-parameters (selected-frame))
				    color-theme-legal-frame-parameters))
	(param)
	(new-params))
    (while params
      (setq param (car params))
      (setq params (cdr params))
      (unless (member param master-params)
	(add-to-list 'new-params param)))
    (sort new-params (lambda (a b) (string< (symbol-name (car a)) 
 					    (symbol-name (car b)))))))

(defun color-theme-get-vars (&optional master-vars)
  "Return a list of variable settings usable in a color theme.
Such an alist may be installed by `color-theme-install-variables'.  The
variables returned must be user variables (this is determined using
`user-variable-p'), and their names must match `color-theme-legal-variables'.

If the optional argument MASTER-VARS is provided, then the alist returned
will only contain variables with settings differing from MASTER-VARS."
  (let ((vars)
	(val))
    (mapatoms (lambda (v)
		(and (user-variable-p v)
		     (string-match color-theme-legal-variables
				   (symbol-name v))
		     (setq val (eval v))
		     (unless (member (cons v val) master-vars)
		       (add-to-list 'vars (cons v val))))))
    (sort vars (lambda (a b) (string< (car a) (car b))))))

(defun color-theme-print-alist (func &rest args)
  "Print an alist returned by function FUNC.
Possible functions might be `color-theme-get-vars' or 
`color-theme-get-params'.  The optional arguments will
be passed along to the function."
  (let ((alist (apply func args))
	(elem))
    (insert "\n     " (if alist "(" "nil"))
    (while alist
      (setq elem (car alist))
      (setq alist (cdr alist))
      (when (= (preceding-char) ?\)) (insert "\n      "))
      (prin1 elem (current-buffer)))
    (when (= (preceding-char) ?\)) (insert ")"))))

;; This function is called from `face-spec-match-p'.  The current
;; implementation in Emacs 20.6 is broken, therefore I provide a
;; replacement function.  This replacement is necessary to better factor
;; two color themes.  This relates to the optional parameter
;; `master-faces' in the function `color-theme-get-faces' -- without
;; this replacement function, color-theme-get-faces will return faces
;; with strings in their specs even if the strings are `equal' -- the
;; strings in the spec are not `eq'!.
(defun face-attr-match-1 (face frame plist property function &optional defaultval)
  "This function was redefined in color-theme.el.
Now it uses `equal' instead of `eq' to compare face attributes with spec."
  (while (and plist (not (eq (car plist) property)))
    (setq plist (cdr (cdr plist))))
  (equal (funcall function face frame)
	 (if plist
	     (nth 1 plist)
	   (or defaultval
	       (funcall function 'default frame)))))

(defun color-theme-get-faces (&optional master-faces)
  "Return a list of faces usable in a color theme.
Such an alist may be installed by `color-theme-install-faces'.  The
faces returned must not match `color-theme-illegal-faces'.

If the optional argument MASTER-FACES is provided, then the list returned
will only contain faces with settings differing from MASTER-FACES."
  (let ((faces (color-theme-filter (face-list) color-theme-illegal-faces t))
	(new-faces))
    ;; Put default face first, therefore default must be the last face
    ;; added to the front of the list.
    (while faces
      (setq face (car faces))
      (setq faces (cdr faces))
      (unless (or (eq face 'default)
		  (face-spec-match-p face (cadr (assoc face master-faces))))
	(add-to-list 'new-faces face)))
    (setq new-faces (sort new-faces 'string-lessp))
    (unless (face-spec-match-p 'default (cadr (assoc 'default master-faces)))
      (add-to-list 'new-faces 'default))
    new-faces))

(defun color-theme-print-faces (&optional master-faces)
  "Print face settings for all faces returned by `color-theme-get-faces'.
Optional argument MASTER-FACES is passed along to  `color-theme-get-faces'."
  (let ((faces (color-theme-get-faces master-faces))
	(face))
    (when faces (insert "\n    "))
    (while faces
      (setq face (car faces))
      (setq faces (cdr faces))
      (when (= (preceding-char) ?\)) (insert "\n    "))
      (prin1 (color-theme-spec face) (current-buffer)))))

(defun color-theme-print (&optional arg)
  "Print the current color theme function.

You can contribute this function to <URL:news:gnu.emacs.sources> or
paste it into your .emacs file and call it.  That should recreate all
the settings necessary for your color theme.

Example:

    \(require 'color-theme)
    \(defun my-color-theme ()
      \"Color theme by Alex Schroeder, created 2000-05-17.\"
      \(interactive)
      \(color-theme-install
       '(...
	 ...
	 ...)))
    \(my-color-theme)

If you want to use a specific color theme function, you can call the
color theme function in your .emacs directly.

Example:

    \(require 'color-theme)
    \(color-theme-gnome2)

If called with a prefix argument, this function tries to create a diff
color theme compared to the last color theme installed.  The last color
theme installed is stored in the variable `color-theme-installed'.  The
default behaviour is a full dump of all relevant settings.  If you
already have two color themes and want to make one of them the parent of
the other, be sure to use a numeric prefix when you install the second
color theme.  See `color-theme-install' for more information."
  (interactive "P")
  (message "Pretty printing current color theme function...")
  (switch-to-buffer (get-buffer-create "*Color Theme*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((master-name)
	(master-theme))
    ;; Set master theme, making sure that the color theme function is
    ;; actually the one mentioned in the theme itself.
    (when (and arg
	       color-theme-installed
	       (equal color-theme-installed 
		      (car (get 'color-theme-installed 'color-theme))))
      (setq master-name (symbol-name color-theme-installed)
	    master-theme (get 'color-theme-installed 'color-theme)))
    ;; insert defun
    (insert "(defun my-color-theme ()\n"
	    "  \"Color theme by " 
	    (if (string= "" user-full-name) user-login-name user-full-name)
	    ", created " (format-time-string "%Y-%m-%d") "."
	    (if master-name (format "\nBased on %s." master-name) "")
	    "\"\n"
	    "  (interactive)\n"
	    (if master-name (format "  (%s)\n" master-name) "")
	    "  (color-theme-install\n"
	    "   '(my-color-theme")
    ;; alist of frame parameters
    (color-theme-print-alist 'color-theme-get-params (nth 1 master-theme))
    ;; alist of variables
    (color-theme-print-alist 'color-theme-get-vars (nth 2 master-theme))
    ;; remaining elements of snapshot: face specs
    (color-theme-print-faces (cdr (cddr master-theme)))
    (insert ")))"))
  (emacs-lisp-mode)
  (goto-char (point-min))
  (message "Pretty printing current color theme function... done"))

;;; Installation of a color theme

(defun color-theme-install-frame-params (params)
  "Change frame parameters using alist PARAMETERS.  
Change all frames and `default-frame-alist'.  The value of
`initial-frame-alist' is not modified.

Called from `color-theme-install'.  See the elisp manual for more
information on frame parameters.

Example: '((foreground-color . \"white\") (background-color . \"black\"))"
  (let ((params (color-theme-filter
		 params color-theme-legal-frame-parameters))
	(frames (frame-list))
	(frame))
    (while frames
      (setq frame (car frames))
      (setq frames (cdr frames))
      (modify-frame-parameters frame params)))
  (setq default-frame-alist (append params default-frame-alist)))

(defun color-theme-install-variables (vars)
  "Change variables using alist VARS.  
All user variables matching `color-theme-legal-variables' are set.
User variables are identified using `user-variable-p'.

Called from `color-theme-install'.

Example: '((apropos-symbol-face . info-xref)
           \(help-highlight-face . info-xref))"
  (let ((vars (color-theme-filter 
	       vars color-theme-legal-variables))
	(var))
    (while vars
      (setq var (car vars))
      (setq vars (cdr vars))
      (set (car var) (cdr var)))))

(defun color-theme-install-faces (faces)
  "Change faces using FACES.

Change faces for all frames and create any faces listed in FACES which
don't exist.  The modified faces will be marked as \"unchanged from
its standard setting\".  This is OK, since the changes made by
installing a color theme should never by saved in .emacs by
customization code.

FACES should be a list where each entry has the form:

  (FACE SPEC)

See `defface' for the format of SPEC.

Called from `color-theme-install'."
  (let ((faces (color-theme-filter faces color-theme-illegal-faces t)))
    (while faces
      (let* ((entry (car faces))
	     (face (nth 0 entry))
	     (spec (nth 1 entry)))
	(or (facep face)
	    (make-empty-face face))
	(face-spec-set face spec)
	(put face 'face-defface-spec spec)
	(setq faces (cdr faces))))))

;; `custom-set-faces' is unusable here because it doesn't allow to set
;; the faces for one frame only.

;; Since FRAME is nil, the face is created and marked as a customized
;; face.  This is achieved by setting the `face-defface-spec'
;; property.  If we don't, new frames will not be created using the
;; face we installed because `face-spec-set' is broken: If given a
;; FRAME of nil, it will not set the default faces; instead it will
;; walk through all the frames and set modify the faces.  If we do set
;; a property (saved-face or face-defface-spec), `make-frame' will
;; correctly use the faces we defined with our color theme.  If we
;; used the property `saved-face', `customize-save-customized' will
;; save all the faces installed as part of a color-theme in .emacs.
;; That's why we use the `face-defface-spec' property.

(defvar color-theme-installed nil
  "Color theme function used to install the current color theme.
Calling this function should reset the current color theme in case 
it has been modified.

The entire theme definition is stored in the property `color-theme' of
this variable.  The theme definition is the same one that was used in
the call to `color-theme-install'.")

(defun color-theme-install (theme)
  "Install a color theme defined by frame parameters, variables and faces.

The theme is installed for all present and future frames; any missing
faces are created.  See `color-theme-install-faces'.

THEME is a color theme definition.  See below for more information.

If you want to install a color theme from your .emacs, use the output
generated by `color-theme-print'.  This produces color theme function
which you can copy to your .emacs.

A color theme definition is a list:
\([FUNCTION] FRAME-PARAMETERS VARIABLE-SETTINGS FACE-DEFINITIONS)

FUNCTION is the color theme function which called color-theme-install.
The function is stored in the variable `color-theme-installed'.  The
entire THEME is stored in property `color-theme' of that variable.  See
below for information on how to prevent this, and why you would want to
do this.

FRAME-PARAMETERS is an alist of frame parameters.  These are installed
with `color-theme-install-frame-params'.

VARIABLE-DEFINITIONS is an alist of variable settings.  These are
installed with `color-theme-install-variables'.

FACE-DEFINITIONS is an alist of face definitions.  These are installed
with `color-theme-install-faces'.

If you want to factor two color themes, you can use a prefix argument
for the second color theme function.  If a color theme function is
called with a prefix argument, then the installation of that color theme
function is not recorded in the variable `color-theme-installed'.
Example: You have two huge color theme functions A and B, and you want
to rewrite B such that it calls A and then installs the differences
between A and B.  This is how to proceed: Start emacs -q, load
color-theme, install A, install B with a numeric prefix, print it with a
numeric prefix:

M-x load-library RET ~/elisp/color-theme.el RET
M-x color-theme-gnome2 RET
C-u M-x color-theme-subtle-hacker RET
C-u M-x color-theme-print RET

Happy hacking."
  ;; optional color theme function (for backwards compatibility)
  (when (functionp (car theme))
    (when (null current-prefix-arg)
      (setq color-theme-installed (car theme))
      (put 'color-theme-installed 'color-theme theme))
    (setq theme (cdr theme)))
  ;; frame parameters
  (color-theme-install-frame-params (car theme))
  (setq theme (cdr theme))
  ;; optional variable defintions (for backwards compatibility)
  (when (listp (caar theme))
    (color-theme-install-variables (car theme))
    (setq theme (cdr theme)))
  ;; face definitions
  (color-theme-install-faces theme))



;;; The color theme functions

(defun color-theme-gnome ()
  "Wheat on darkslategrey scheme
from one version of Emacs in RH6 and Gnome, modified by Jonadab."
  (interactive)
  (color-theme-install
   '(color-theme-gnome
     ((foreground-color . "wheat")
      (background-color . "darkslategrey")
      (background-mode . dark)) 
     (default ((t (nil))))
     (region ((t (:foreground "cyan" :background "dark cyan"))))
     (underline ((t (:foreground "yellow" :underline))))
     (modeline ((t (:foreground "dark cyan" :background "wheat"))))
     (italic ((t (:foreground "dark red" :italic))))
     (bold-italic ((t (:foreground "dark red" :bold :italic))))
     (font-lock-comment-face ((t (:foreground "Firebrick"))))
     (bold ((t (:bold)))))))

(defun color-theme-gnome2 ()
  "Wheat on darkslategrey scheme.  `color-theme-gnome' started it all.
On my NT Emacs with a true color display it looks very somber and dark.
On my Linux I only get to choose from 64 colors and the faces look
rather bright on their darkslategrey background.

This theme supports standard faces, font-lock, eshell, info, message,
gnus, custom, widget, woman, diary, cperl, and erc.

This color theme does not support w3 faces because w3 faces can be
controlled by your default style sheet.

This is what you should put in your .Xdefaults file, if you want to
change the colors of the menus as well:

emacs*Background:		DarkSlateGray
emacs*Foreground:		Wheat"
  (interactive)
  (color-theme-install
   '(color-theme-gnome2
     ((foreground-color . "wheat")
      (background-color . "darkslategrey")
      (mouse-color . "Grey")
      (cursor-color . "LightGray")
      (border-color . "black")
      (background-mode . dark))
     ((apropos-keybinding-face . underline)
      (apropos-label-face . (face italic mouse-face highlight))
      (apropos-match-face . secondary-selection)
      (apropos-property-face . bold-italic)
      (apropos-symbol-face . info-xref)
      (goto-address-mail-face . message-header-to-face)
      (goto-address-mail-mouse-face . secondary-selection)
      (goto-address-url-face . info-xref)
      (goto-address-url-mouse-face . highlight)
      (list-matching-lines-face . bold)
      (view-highlight-face . highlight))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t :foreground "beige"))))
     (calendar-today-face ((t (:underline t))))
     (cperl-array-face ((t (:foreground "Yellow"))))
     (cperl-hash-face ((t (:foreground "White"))))
     (cperl-nonoverridable-face ((t (:foreground "SkyBlue"))))
     (custom-button-face ((t (:underline t :foreground "MediumSlateBlue"))))
     (custom-documentation-face ((t (:foreground "Grey"))))
     (custom-group-tag-face ((t (:foreground "MediumAquamarine"))))
     (custom-state-face ((t (:foreground "LightSalmon"))))
     (custom-variable-tag-face ((t (:foreground "Aquamarine"))))
     (diary-face ((t (:foreground "IndianRed"))))
     (erc-action-face ((t (:bold t))))
     (erc-bold-face ((t (:bold t))))
     (erc-default-face ((t (nil))))
     (erc-direct-msg-face ((t (:foreground "LightSalmon"))))
     (erc-error-face ((t (:bold t :foreground "IndianRed"))))
     (erc-input-face ((t (:foreground "Beige"))))
     (erc-inverse-face ((t (:background "wheat" :foreground "darkslategrey"))))
     (erc-notice-face ((t (:foreground "MediumAquamarine"))))
     (erc-pal-face ((t (:foreground "pale green"))))
     (erc-prompt-face ((t (:foreground "MediumAquamarine"))))
     (erc-underline-face ((t (:underline t))))
     (eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))
     (eshell-ls-backup-face ((t (:foreground "Grey"))))
     (eshell-ls-clutter-face ((t (:foreground "DimGray"))))
     (eshell-ls-directory-face ((t (:bold t :foreground "MediumSlateBlue"))))
     (eshell-ls-executable-face ((t (:foreground "Coral"))))
     (eshell-ls-missing-face ((t (:foreground "black"))))
     (eshell-ls-picture-face ((t (:foreground "Violet"))))
     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))
     (eshell-ls-readonly-face ((t (:foreground "Aquamarine"))))
     (eshell-ls-special-face ((t (:foreground "Gold"))))
     (eshell-ls-symlink-face ((t (:foreground "White"))))
     (eshell-ls-unreadable-face ((t (:foreground "DimGray"))))
     (eshell-prompt-face ((t (:foreground "MediumAquamarine"))))
     (font-lock-builtin-face ((t (:bold t :foreground "PaleGreen"))))
     (font-lock-comment-face ((t (:foreground "LightBlue"))))
     (font-lock-constant-face ((t (:foreground "Aquamarine"))))
     (font-lock-function-name-face ((t (:bold t :foreground "Aquamarine"))))
     (font-lock-keyword-face ((t (:foreground "Salmon"))))
     (font-lock-string-face ((t (:foreground "LightSalmon"))))
     (font-lock-type-face ((t (:bold t :foreground "YellowGreen"))))
     (font-lock-variable-name-face ((t (:bold t :foreground "Aquamarine"))))
     (font-lock-warning-face ((t (:bold t :foreground "IndianRed"))))
     (gnus-cite-face-1 ((t (:foreground "LightSalmon"))))
     (gnus-cite-face-2 ((t (:foreground "Khaki"))))
     (gnus-cite-face-3 ((t (:foreground "Coral"))))
     (gnus-cite-face-4 ((t (:foreground "yellow green"))))
     (gnus-cite-face-5 ((t (:foreground "IndianRed"))))
     (gnus-group-mail-1-empty-face ((t (:foreground "White"))))
     (gnus-group-mail-1-face ((t (:bold t :foreground "White"))))
     (gnus-group-mail-2-empty-face ((t (:foreground "light cyan"))))
     (gnus-group-mail-2-face ((t (:bold t :foreground "light cyan"))))
     (gnus-group-mail-3-empty-face ((t (:foreground "LightBlue"))))
     (gnus-group-mail-3-face ((t (:bold t :foreground "LightBlue"))))
     (gnus-group-mail-low-empty-face ((t (:foreground "Aquamarine"))))
     (gnus-group-mail-low-face ((t (:bold t :foreground "Aquamarine"))))
     (gnus-group-news-1-empty-face ((t (:foreground "White"))))
     (gnus-group-news-1-face ((t (:bold t :foreground "White"))))
     (gnus-group-news-2-empty-face ((t (:foreground "light cyan"))))
     (gnus-group-news-2-face ((t (:bold t :foreground "light cyan"))))
     (gnus-group-news-3-empty-face ((t (:foreground "LightBlue"))))
     (gnus-group-news-3-face ((t (:bold t :foreground "LightBlue"))))
     (gnus-group-news-4-empty-face ((t (:foreground "Aquamarine"))))
     (gnus-group-news-4-face ((t (:bold t :foreground "Aquamarine"))))
     (gnus-group-news-5-empty-face ((t (:foreground "MediumAquamarine"))))
     (gnus-group-news-5-face ((t (:bold t :foreground "MediumAquamarine"))))
     (gnus-group-news-6-empty-face ((t (:foreground "MediumAquamarine"))))
     (gnus-group-news-6-face ((t (:bold t :foreground "MediumAquamarine"))))
     (gnus-group-news-low-empty-face ((t (:foreground "MediumAquamarine"))))
     (gnus-group-news-low-face ((t (:bold t :foreground "MediumAquamarine"))))
     (gnus-header-content-face ((t (:foreground "LightSkyBlue3"))))
     (gnus-header-from-face ((t (:bold t :foreground "light cyan"))))
     (gnus-header-name-face ((t (:bold t :foreground "LightBlue"))))
     (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "MediumAquamarine"))))
     (gnus-header-subject-face ((t (:bold t :foreground "light cyan"))))
     (gnus-signature-face ((t (:foreground "Grey"))))
     (gnus-splash-face ((t (:foreground "ForestGreen"))))
     (gnus-summary-cancelled-face ((t (:background "Black" :foreground "Yellow"))))
     (gnus-summary-high-ancient-face ((t (:bold t :foreground "MediumAquamarine"))))
     (gnus-summary-high-read-face ((t (:bold t :foreground "Aquamarine"))))
     (gnus-summary-high-ticked-face ((t (:bold t :foreground "LightSalmon"))))
     (gnus-summary-high-unread-face ((t (:italic t :bold t :foreground "beige"))))
     (gnus-summary-low-ancient-face ((t (:italic t :foreground "DimGray"))))
     (gnus-summary-low-read-face ((t (:foreground "Grey"))))
     (gnus-summary-low-ticked-face ((t (:foreground "Pink"))))
     (gnus-summary-low-unread-face ((t (:foreground "LightGray"))))
     (gnus-summary-normal-ancient-face ((t (:foreground "MediumAquamarine"))))
     (gnus-summary-normal-read-face ((t (:foreground "Aquamarine"))))
     (gnus-summary-normal-ticked-face ((t (:foreground "LightSalmon"))))
     (gnus-summary-normal-unread-face ((t (nil))))
     (gnus-summary-selected-face ((t (:underline t))))
     (highlight ((t (:background "PaleGreen" :foreground "DarkGreen"))))
     (highline-face ((t (:background "SeaGreen"))))
     (holiday-face ((t (:background "DimGray"))))
     (info-menu-5 ((t (:underline t))))
     (info-node ((t (:underline t :bold t :foreground "DodgerBlue1"))))
     (info-xref ((t (:underline t :foreground "DodgerBlue1"))))
     (italic ((t (:italic t))))
     (message-cited-text-face ((t (:foreground "LightSalmon"))))
     (message-header-cc-face ((t (:foreground "light cyan"))))
     (message-header-name-face ((t (:foreground "LightBlue"))))
     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "MediumAquamarine"))))
     (message-header-other-face ((t (:foreground "MediumAquamarine"))))
     (message-header-subject-face ((t (:bold t :foreground "light cyan"))))
     (message-header-to-face ((t (:bold t :foreground "light cyan"))))
     (message-header-xheader-face ((t (:foreground "MediumAquamarine"))))
     (message-separator-face ((t (:foreground "chocolate"))))
     (modeline ((t (:background "DarkOliveGreen" :foreground "wheat"))))
     (region ((t (:background "dark cyan" :foreground "cyan"))))
     (secondary-selection ((t (:background "Aquamarine" :foreground "SlateBlue"))))
     (show-paren-match-face ((t (:background "Aquamarine" :foreground "SlateBlue"))))
     (show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))
     (underline ((t (:underline t))))
     (widget-field-face ((t (:foreground "LightBlue"))))
     (widget-inactive-face ((t (:foreground "DimGray"))))
     (widget-single-line-field-face ((t (:foreground "LightBlue"))))
     (woman-bold-face ((t (:bold t))))
     (woman-italic-face ((t (:foreground "beige"))))
     (woman-unknown-face ((t (:foreground "LightSalmon")))))))

(defun color-theme-simple-1 ()
  "Black bg; doesn't mess with most faces, but does turn on dark
background mode."
  (interactive)
  (color-theme-install
   '(color-theme-simple-1
     ((foreground-color . "white")
      (background-color . "black")
      (cursor-color     . "indian red")
      (background-mode  . dark))
     (default ((t (nil))))
     (modeline ((t (:foreground "black" :background "white"))))
     (underline ((t (:underline t))))
     (region ((t (:background "grey")))))))

(defun color-theme-jonadabian ()
  "Dark blue background.
Supports standard faces, font-lock, highlight-changes, widget and
custom."
  (interactive)
  (color-theme-install
   '(color-theme-jonadabian
     ((foreground-color . "wheat")
      (cursor-color . "medium turquoise")
      (background-color . "#000055")
      (background-mode . dark))
     (default ((t (:foreground "#CCBB77" :background "#000055"))))
     (modeline ((t (:foreground "cyan" :background "#007080"))))
     (underline ((t (:underline t))))
     (region ((t (:background "#004080"))))
     (font-lock-keyword-face ((t (:foreground "#00BBBB"))))
     (font-lock-comment-face ((t (:foreground "grey50" :bold t :italic t))))
     (font-lock-string-face ((t (:foreground "#10D010"))))
     (font-lock-constant-face ((t (:foreground "indian red"))))
     (highlight-changes-face ((t (:background "navy"))))
     (highlight-changes-delete-face ((t (:foreground "red" :background "navy"))))
     (widget-field-face ((t (:foreground "black" :background "grey35"))))
     (widget-inactive-face ((t (:foreground "gray"))))
     (custom-button-face ((t (:foreground "yellow" :background "dark blue"))))
     (custom-state-face ((t (:foreground "mediumaquamarine"))))
     (custom-face-tag-face ((t (:foreground "goldenrod" :underline t))))
     (custom-documentation-face ((t (:foreground "#10D010"))))
     (custom-set-face ((t (:foreground "#2020D0")))))))

(defun color-theme-ryerson ()
  "White on midnightblue scheme used at Ryerson Polytechnic University
in the Electronic Engineering department."
  (interactive)
  (color-theme-install
   '(color-theme-ryerson
     ((foreground-color . "white")
      (background-color . "midnightblue")
      (cursor-color     . "red")
      (background-mode  . dark))
     (default ((t (nil))))
     (modeline ((t (:foreground "black" :background "slategray3"))))
     (underline ((t (:underline t))))
     (region ((t (:foreground "black" :background "slategray3")))))))

(defun color-theme-wheat ()
  "Default colors on a wheat background.
Calls the standard color theme function `color-theme-standard' in order
to reset all faces."
  (interactive)
  (color-theme-standard)
  (setq color-theme-installed 'color-theme-wheat)
  (color-theme-install-frame-params '((background-color . "Wheat"))))

(defun color-theme-standard ()
  "The default colors.  Use this theme to undo it all.
Includes all the faces in the Emacs source created using `defface' as
well as the eshell faces.  This is also a good starting place for
would-be theme authors."
;; In order to produce this, follow these two steps:
;; 1. cd into the Emacs lisp directory and run the following command:
;;    ( for d in `find -type d`; do grep --files-with-matches 'defface[ 	]' $d/*.el; done ) | sort | uniq
;;    Put the result in a lisp block:
;;     (progn
;;       (load-library "calendar")
;;       (load-library "cus-edit")
;;       (load-library "custom")
;;       (load-library "ediff-init")
;;       (load-library "viper-init")
;;       (load-library "enriched")
;;       (load-library "faces")
;;       (load-library "font-lock")
;;       (load-library "gnus-art")
;;       (load-library "gnus-cite")
;;       (load-library "gnus")
;;       (load-library "message")
;;       (load-library "hilit-chg")
;;       (load-library "info")
;;       (load-library "paren")
;;       (load-library "cperl-mode")
;;       (load-library "make-mode")
;;       (load-library "vhdl-mode")
;;       (load-library "verilog-mode")
;;       (load-library "speedbar")
;;       (load-library "flyspell")
;;       (load-library "vcursor")
;;       (load-library "wid-edit"))
;;    Repeat this for any directories on your load path which you want to
;;    include in the standard.  This might include W3, eshell, etc.
;;     (progn
;;       (load-library "eshell-test")
;;       (load-library "eshell-prompt")
;;       (load-library "eshell-ls"))
;; 2. Start emacs using the --no-init-file and --no-site-file command line
;;    arguments.  Evaluate the lisp block you prepared.
;; 3. Load color-theme and run color-theme-print.  Save the output and use it 
;;    to define color-theme-standard.
  (interactive)
  ;; Note that some of the things that make up a color theme are
  ;; actually variable settings!
  (color-theme-install
   '(color-theme-standard
     ((foreground-color . "black")
      (background-color . "white")
      (mouse-color . "black")
      (cursor-color . "black")
      (border-color . "black")
      (background-mode . light))
     ((apropos-keybinding-face . underline)
      (apropos-label-face . (face italic mouse-face highlight))
      (apropos-match-face . secondary-selection)
      (apropos-property-face . bold-italic)
      (apropos-symbol-face . bold)
      (goto-address-mail-face . italic)
      (goto-address-mail-mouse-face . secondary-selection)
      (goto-address-url-face . bold)
      (goto-address-url-mouse-face . highlight)
      (help-highlight-face . underline)
      (list-matching-lines-face . bold)
      (view-highlight-face . highlight))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t :italic t))))
     (calendar-today-face ((t (:underline t))))
     (cperl-array-face ((t (:foreground "Blue" :background "lightyellow2" :bold t))))
     (cperl-hash-face ((t (:foreground "Red" :background "lightyellow2" :bold t :italic t))))
     (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))
     (custom-button-face ((t (nil))))
     (custom-changed-face ((t (:foreground "white" :background "blue"))))
     (custom-documentation-face ((t (nil))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face ((t (:foreground "blue" :underline t))))
     (custom-group-tag-face-1 ((t (:foreground "red" :underline t))))
     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))
     (custom-modified-face ((t (:foreground "white" :background "blue"))))
     (custom-rogue-face ((t (:foreground "pink" :background "black"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face ((t (:foreground "blue" :background "white"))))
     (custom-state-face ((t (:foreground "dark green"))))
     (custom-variable-button-face ((t (:bold t :underline t))))
     (custom-variable-tag-face ((t (:foreground "blue" :underline t))))
     (default ((t (nil))))
     (diary-face ((t (:foreground "red"))))
     (ediff-current-diff-face-A ((t (:foreground "firebrick" :background "pale green"))))
     (ediff-current-diff-face-Ancestor ((t (:foreground "Black" :background "VioletRed"))))
     (ediff-current-diff-face-B ((t (:foreground "DarkOrchid" :background "Yellow"))))
     (ediff-current-diff-face-C ((t (:foreground "Navy" :background "Pink"))))
     (ediff-even-diff-face-A ((t (:foreground "Black" :background "light grey"))))
     (ediff-even-diff-face-Ancestor ((t (:foreground "White" :background "Grey"))))
     (ediff-even-diff-face-B ((t (:foreground "White" :background "Grey"))))
     (ediff-even-diff-face-C ((t (:foreground "Black" :background "light grey"))))
     (ediff-fine-diff-face-A ((t (:foreground "Navy" :background "sky blue"))))
     (ediff-fine-diff-face-Ancestor ((t (:foreground "Black" :background "Green"))))
     (ediff-fine-diff-face-B ((t (:foreground "Black" :background "cyan"))))
     (ediff-fine-diff-face-C ((t (:foreground "Black" :background "Turquoise"))))
     (ediff-odd-diff-face-A ((t (:foreground "White" :background "Grey"))))
     (ediff-odd-diff-face-Ancestor ((t (:foreground "Black" :background "light grey"))))
     (ediff-odd-diff-face-B ((t (:foreground "Black" :background "light grey"))))
     (ediff-odd-diff-face-C ((t (:foreground "White" :background "Grey"))))
     (eshell-ls-archive-face ((t (:foreground "Orchid" :bold t))))
     (eshell-ls-backup-face ((t (:foreground "OrangeRed"))))
     (eshell-ls-clutter-face ((t (:foreground "OrangeRed" :bold t))))
     (eshell-ls-directory-face ((t (:foreground "Blue" :bold t))))
     (eshell-ls-executable-face ((t (:foreground "ForestGreen" :bold t))))
     (eshell-ls-missing-face ((t (:foreground "Red" :bold t))))
     (eshell-ls-product-face ((t (:foreground "OrangeRed"))))
     (eshell-ls-readonly-face ((t (:foreground "Brown"))))
     (eshell-ls-special-face ((t (:foreground "Magenta" :bold t))))
     (eshell-ls-symlink-face ((t (:foreground "DarkCyan" :bold t))))
     (eshell-ls-unreadable-face ((t (:foreground "Grey30"))))
     (eshell-prompt-face ((t (:foreground "Red" :bold t))))
     (eshell-test-failed-face ((t (:foreground "OrangeRed" :bold t))))
     (eshell-test-ok-face ((t (:foreground "Green" :bold t))))
     (excerpt ((t (:italic t))))
     (fixed ((t (:bold t))))
     (flyspell-duplicate-face ((t (:foreground "Gold3" :bold t :underline t))))
     (flyspell-incorrect-face ((t (:foreground "OrangeRed" :bold t :underline t))))
     (font-lock-builtin-face ((t (:foreground "Orchid"))))
     (font-lock-comment-face ((t (:foreground "Firebrick"))))
     (font-lock-constant-face ((t (:foreground "CadetBlue"))))
     (font-lock-function-name-face ((t (:foreground "Blue"))))
     (font-lock-keyword-face ((t (:foreground "Purple"))))
     (font-lock-string-face ((t (:foreground "RosyBrown"))))
     (font-lock-type-face ((t (:foreground "ForestGreen"))))
     (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     (gnus-cite-attribution-face ((t (:italic t))))
     (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))
     (gnus-cite-face-10 ((t (:foreground "medium purple"))))
     (gnus-cite-face-11 ((t (:foreground "turquoise"))))
     (gnus-cite-face-2 ((t (:foreground "firebrick"))))
     (gnus-cite-face-3 ((t (:foreground "dark green"))))
     (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))
     (gnus-cite-face-5 ((t (:foreground "dark khaki"))))
     (gnus-cite-face-6 ((t (:foreground "dark violet"))))
     (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))
     (gnus-cite-face-8 ((t (:foreground "magenta"))))
     (gnus-cite-face-9 ((t (:foreground "violet"))))
     (gnus-emphasis-bold ((t (:bold t))))
     (gnus-emphasis-bold-italic ((t (:bold t :italic t))))
     (gnus-emphasis-italic ((t (:italic t))))
     (gnus-emphasis-underline ((t (:underline t))))
     (gnus-emphasis-underline-bold ((t (:bold t :underline t))))
     (gnus-emphasis-underline-bold-italic ((t (:bold t :italic t :underline t))))
     (gnus-emphasis-underline-italic ((t (:italic t :underline t))))
     (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))
     (gnus-group-mail-1-face ((t (:foreground "DeepPink3" :bold t))))
     (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))
     (gnus-group-mail-2-face ((t (:foreground "HotPink3" :bold t))))
     (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))
     (gnus-group-mail-3-face ((t (:foreground "magenta4" :bold t))))
     (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))
     (gnus-group-mail-low-face ((t (:foreground "DeepPink4" :bold t))))
     (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))
     (gnus-group-news-1-face ((t (:foreground "ForestGreen" :bold t))))
     (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))
     (gnus-group-news-2-face ((t (:foreground "CadetBlue4" :bold t))))
     (gnus-group-news-3-empty-face ((t (nil))))
     (gnus-group-news-3-face ((t (:bold t))))
     (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))
     (gnus-group-news-low-face ((t (:foreground "DarkGreen" :bold t))))
     (gnus-header-content-face ((t (:foreground "indianred4" :italic t))))
     (gnus-header-from-face ((t (:foreground "red3"))))
     (gnus-header-name-face ((t (:foreground "maroon"))))
     (gnus-header-newsgroups-face ((t (:foreground "MidnightBlue" :italic t))))
     (gnus-header-subject-face ((t (:foreground "red4"))))
     (gnus-signature-face ((t (:italic t))))
     (gnus-splash-face ((t (:foreground "ForestGreen"))))
     (gnus-summary-cancelled-face ((t (:foreground "yellow" :background "black"))))
     (gnus-summary-high-ancient-face ((t (:foreground "RoyalBlue" :bold t))))
     (gnus-summary-high-read-face ((t (:foreground "DarkGreen" :bold t))))
     (gnus-summary-high-ticked-face ((t (:foreground "firebrick" :bold t))))
     (gnus-summary-high-unread-face ((t (:bold t))))
     (gnus-summary-low-ancient-face ((t (:foreground "RoyalBlue" :italic t))))
     (gnus-summary-low-read-face ((t (:foreground "DarkGreen" :italic t))))
     (gnus-summary-low-ticked-face ((t (:foreground "firebrick" :italic t))))
     (gnus-summary-low-unread-face ((t (:italic t))))
     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))
     (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))
     (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))
     (gnus-summary-normal-unread-face ((t (nil))))
     (gnus-summary-selected-face ((t (:underline t))))
     (highlight ((t (:background "darkseagreen2"))))
     (highlight-changes-delete-face ((t (:foreground "red" :underline t))))
     (highlight-changes-face ((t (:foreground "red"))))
     (highline-face ((t (:background "paleturquoise"))))
     (holiday-face ((t (:background "pink"))))
     (info-menu-5 ((t (:underline t))))
     (info-node ((t (:bold t :italic t))))
     (info-xref ((t (:bold t))))
     (italic ((t (:italic t))))
     (makefile-space-face ((t (:background "hotpink"))))
     (message-cited-text-face ((t (:foreground "red"))))
     (message-header-cc-face ((t (:foreground "MidnightBlue"))))
     (message-header-name-face ((t (:foreground "cornflower blue"))))
     (message-header-newsgroups-face ((t (:foreground "blue4" :bold t :italic t))))
     (message-header-other-face ((t (:foreground "steel blue"))))
     (message-header-subject-face ((t (:foreground "navy blue" :bold t))))
     (message-header-to-face ((t (:foreground "MidnightBlue" :bold t))))
     (message-header-xheader-face ((t (:foreground "blue"))))
     (message-separator-face ((t (:foreground "brown"))))
     (modeline ((t (:foreground "white" :background "black"))))
     (region ((t (:background "gray"))))
     (secondary-selection ((t (:background "paleturquoise"))))
     (show-paren-match-face ((t (:background "turquoise"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "purple"))))
     (speedbar-button-face ((t (:foreground "green4"))))
     (speedbar-directory-face ((t (:foreground "blue4"))))
     (speedbar-file-face ((t (:foreground "cyan4"))))
     (speedbar-highlight-face ((t (:background "green"))))
     (speedbar-selected-face ((t (:foreground "red" :underline t))))
     (speedbar-tag-face ((t (:foreground "brown"))))
     (term-black ((t (:foreground "black"))))
     (term-blackbg ((t (:background "black"))))
     (term-blue ((t (:foreground "blue"))))
     (term-bluebg ((t (:background "blue"))))
     (term-bold ((t (:bold t))))
     (term-cyan ((t (:foreground "cyan"))))
     (term-cyanbg ((t (:background "cyan"))))
     (term-default-bg ((t (nil))))
     (term-default-bg-inv ((t (nil))))
     (term-default-fg ((t (nil))))
     (term-default-fg-inv ((t (nil))))
     (term-green ((t (:foreground "green"))))
     (term-greenbg ((t (:background "green"))))
     (term-invisible ((t (nil))))
     (term-invisible-inv ((t (nil))))
     (term-magenta ((t (:foreground "magenta"))))
     (term-magentabg ((t (:background "magenta"))))
     (term-red ((t (:foreground "red"))))
     (term-redbg ((t (:background "red"))))
     (term-underline ((t (:underline t))))
     (term-white ((t (:foreground "white"))))
     (term-whitebg ((t (:background "white"))))
     (term-yellow ((t (:foreground "yellow"))))
     (term-yellowbg ((t (:background "yellow"))))
     (underline ((t (:underline t))))
     (vcursor ((t (:foreground "blue" :background "cyan" :underline t))))
     (vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))
     (vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))
     (vhdl-font-lock-enumvalue-face ((t (:foreground "Gold4"))))
     (vhdl-font-lock-function-face ((t (:foreground "Orchid4"))))
     (vhdl-font-lock-prompt-face ((t (:foreground "Red" :bold t))))
     (vhdl-font-lock-reserved-words-face ((t (:foreground "Orange" :bold t))))
     (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))
     (vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))
     (vhdl-speedbar-architecture-selected-face ((t (:foreground "Blue" :underline t))))
     (vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))
     (vhdl-speedbar-configuration-selected-face ((t (:foreground "DarkGoldenrod" :underline t))))
     (vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))
     (vhdl-speedbar-entity-selected-face ((t (:foreground "ForestGreen" :underline t))))
     (vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))
     (vhdl-speedbar-instantiation-selected-face ((t (:foreground "Brown" :underline t))))
     (vhdl-speedbar-package-face ((t (:foreground "Grey50"))))
     (vhdl-speedbar-package-selected-face ((t (:foreground "Grey50" :underline t))))
     (viper-minibuffer-emacs-face ((t (:foreground "Black" :background "darkseagreen2"))))
     (viper-minibuffer-insert-face ((t (:foreground "Black" :background "pink"))))
     (viper-minibuffer-vi-face ((t (:foreground "DarkGreen" :background "grey"))))
     (viper-replace-overlay-face ((t (:foreground "Black" :background "darkseagreen2"))))
     (viper-search-face ((t (:foreground "Black" :background "khaki"))))
     (widget-button-face ((t (:bold t))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "dark green"))))
     (widget-field-face ((t (:background "gray85"))))
     (widget-inactive-face ((t (:foreground "dim gray"))))
     (widget-single-line-field-face ((t (:background "gray85")))))))

(defun color-theme-fischmeister ()
  "The light colors on a grey blackground.
Sebastian Fischmeister <sfischme@nexus.lzk.tuwien.ac.at>"
  (interactive)
  (color-theme-install
   '(color-theme-fischmeister
     ((foreground-color . "black")
      (background-color . "gray80")
      (mouse-color . "red")
      (cursor-color . "yellow")
      (border-color . "black")
      (background-mode . light))
     (default ((t (nil))))
     (modeline ((t (:foreground "gray80" :background "black"))))
     (highlight ((t (:background "darkseagreen2"))))
     (bold ((t (:bold t))))
     (italic ((t (:italic t))))
     (bold-italic ((t (:bold t :italic t))))
     (region ((t (:background "gray"))))
     (secondary-selection ((t (:background "paleturquoise"))))
     (underline ((t (:underline t))))
     (show-paren-match-face ((t (:foreground "yellow" :background "darkgreen"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "red"))))
     (font-lock-comment-face ((t (:foreground "FireBrick" :bold t :italic t))))
     (font-lock-string-face ((t (:foreground "DarkSlateBlue" :italic t))))
     (font-lock-keyword-face ((t (:foreground "navy"))))
     (font-lock-builtin-face ((t (:foreground "white"))))
     (font-lock-function-name-face ((t (:foreground "Blue"))))
     (font-lock-variable-name-face ((t (:foreground "Darkblue"))))
     (font-lock-type-face ((t (:foreground "darkgreen"))))
     (font-lock-constant-face ((t (:foreground "CadetBlue"))))
     (font-lock-warning-face ((t (:foreground "Orchid" :bold t))))
     (font-lock-reference-face ((t (:foreground "SteelBlue")))))))

(defun color-theme-sitaramv-solaris ()
  "White on a midnight blue background.  Lots of yellow and orange.
Includes faces for font-lock, widget, custom, speedbar, message, gnus,
eshell."
  (interactive)
  (color-theme-install
   '(color-theme-sitaramv-solaris
     ((foreground-color . "white")
      (background-color . "MidnightBlue")
      (mouse-color . "yellow")
      (cursor-color . "magenta2")
      (border-color . "black")
      (background-mode . dark))
     (default ((t (nil))))
     (modeline ((t (:foreground "black" :background "gold2"))))
     (highlight ((t (:foreground "black" :background "Aquamarine"))))
     (bold ((t (:bold t))))
     (italic ((t (:italic t))))
     (bold-italic ((t (:bold t :italic t))))
     (region ((t (:foreground "black" :background "snow3"))))
     (secondary-selection ((t (:foreground "black" :background "aquamarine"))))
     (underline ((t (:underline t))))
     (lazy-highlight-face ((t (:foreground "yellow"))))
     (font-lock-comment-face ((t (:foreground "orange" :italic t))))
     (font-lock-string-face ((t (:foreground "orange"))))
     (font-lock-keyword-face ((t (:foreground "green"))))
     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-function-name-face ((t (:foreground "cyan" :bold t))))
     (font-lock-variable-name-face ((t (:foreground "white"))))
     (font-lock-type-face ((t (:foreground "cyan"))))
     (font-lock-constant-face ((t (:foreground "Aquamarine"))))
     (font-lock-warning-face ((t (:foreground "Pink" :bold t))))
     (widget-documentation-face ((t (:foreground "lime green"))))
     (widget-button-face ((t (:bold t))))
     (widget-field-face ((t (:background "dim gray"))))
     (widget-single-line-field-face ((t (:background "dim gray"))))
     (widget-inactive-face ((t (:foreground "light gray"))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))
     (custom-rogue-face ((t (:foreground "pink" :background "black"))))
     (custom-modified-face ((t (:foreground "white" :background "blue"))))
     (custom-set-face ((t (:foreground "blue" :background "white"))))
     (custom-changed-face ((t (:foreground "white" :background "blue"))))
     (custom-saved-face ((t (:underline t))))
     (custom-button-face ((t (nil))))
     (custom-documentation-face ((t (nil))))
     (custom-state-face ((t (:foreground "lime green"))))
     (custom-variable-tag-face ((t (:foreground "light blue" :underline t))))
     (custom-variable-button-face ((t (:bold t :underline t))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face-1 ((t (:foreground "pink" :underline t))))
     (custom-group-tag-face ((t (:foreground "light blue" :underline t))))
     (speedbar-button-face ((t (:foreground "green3"))))
     (speedbar-file-face ((t (:foreground "cyan"))))
     (speedbar-directory-face ((t (:foreground "light blue"))))
     (speedbar-tag-face ((t (:foreground "yellow"))))
     (speedbar-selected-face ((t (:foreground "red" :underline t))))
     (speedbar-highlight-face ((t (:background "sea green"))))
     (font-lock-doc-string-face ((t (:foreground "Plum1" :bold t))))
     (font-lock-exit-face ((t (:foreground "green"))))
     (ff-paths-non-existant-file-face ((t (:foreground "NavyBlue" :bold t))))
     (show-paren-match-face ((t (:background "red"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "purple"))))
     (message-header-to-face ((t (:foreground "green2" :bold t))))
     (message-header-cc-face ((t (:foreground "LightGoldenrod" :bold t))))
     (message-header-subject-face ((t (:foreground "green3"))))
     (message-header-newsgroups-face ((t (:foreground "yellow" :bold t :italic t))))
     (message-header-other-face ((t (:foreground "Salmon"))))
     (message-header-name-face ((t (:foreground "green3"))))
     (message-header-xheader-face ((t (:foreground "GreenYellow"))))
     (message-separator-face ((t (:foreground "Tan"))))
     (message-cited-text-face ((t (:foreground "Gold"))))
     (message-mml-face ((t (:foreground "ForestGreen"))))
     (gnus-group-news-1-face ((t (:foreground "PaleTurquoise" :bold t))))
     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))
     (gnus-group-news-2-face ((t (:foreground "turquoise" :bold t))))
     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))
     (gnus-group-news-3-face ((t (:bold t))))
     (gnus-group-news-3-empty-face ((t (nil))))
     (gnus-group-news-4-face ((t (:bold t))))
     (gnus-group-news-4-empty-face ((t (nil))))
     (gnus-group-news-5-face ((t (:bold t))))
     (gnus-group-news-5-empty-face ((t (nil))))
     (gnus-group-news-6-face ((t (:bold t))))
     (gnus-group-news-6-empty-face ((t (nil))))
     (gnus-group-news-low-face ((t (:foreground "DarkTurquoise" :bold t))))
     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))
     (gnus-group-mail-1-face ((t (:foreground "aquamarine1" :bold t))))
     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))
     (gnus-group-mail-2-face ((t (:foreground "aquamarine2" :bold t))))
     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))
     (gnus-group-mail-3-face ((t (:foreground "aquamarine3" :bold t))))
     (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))
     (gnus-group-mail-low-face ((t (:foreground "aquamarine4" :bold t))))
     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))
     (gnus-summary-selected-face ((t (:underline t))))
     (gnus-summary-cancelled-face ((t (:foreground "yellow" :background "black"))))
     (gnus-summary-high-ticked-face ((t (:foreground "pink" :bold t))))
     (gnus-summary-low-ticked-face ((t (:foreground "pink" :italic t))))
     (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))
     (gnus-summary-high-ancient-face ((t (:foreground "SkyBlue" :bold t))))
     (gnus-summary-low-ancient-face ((t (:foreground "SkyBlue" :italic t))))
     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))
     (gnus-summary-high-unread-face ((t (:bold t))))
     (gnus-summary-low-unread-face ((t (:italic t))))
     (gnus-summary-normal-unread-face ((t (nil))))
     (gnus-summary-high-read-face ((t (:foreground "PaleGreen" :bold t))))
     (gnus-summary-low-read-face ((t (:foreground "PaleGreen" :italic t))))
     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))
     (gnus-splash-face ((t (:foreground "Brown"))))
     (eshell-ls-directory-face ((t (:foreground "SkyBlue" :bold t))))
     (eshell-ls-symlink-face ((t (:foreground "Cyan" :bold t))))
     (eshell-ls-executable-face ((t (:foreground "Green" :bold t))))
     (eshell-ls-readonly-face ((t (:foreground "Pink"))))
     (eshell-ls-unreadable-face ((t (:foreground "DarkGrey"))))
     (eshell-ls-special-face ((t (:foreground "Magenta" :bold t))))
     (eshell-ls-missing-face ((t (:foreground "Red" :bold t))))
     (eshell-ls-archive-face ((t (:foreground "Orchid" :bold t))))
     (eshell-ls-backup-face ((t (:foreground "LightSalmon"))))
     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))
     (eshell-ls-clutter-face ((t (:foreground "OrangeRed" :bold t))))
     (eshell-prompt-face ((t (:foreground "Pink" :bold t))))
     (term-default-fg ((t (nil))))
     (term-default-bg ((t (nil))))
     (term-default-fg-inv ((t (nil))))
     (term-default-bg-inv ((t (nil))))
     (term-bold ((t (:bold t))))
     (term-underline ((t (:underline t))))
     (term-invisible ((t (nil))))
     (term-invisible-inv ((t (nil))))
     (term-black ((t (:foreground "black"))))
     (term-red ((t (:foreground "red"))))
     (term-green ((t (:foreground "green"))))
     (term-yellow ((t (:foreground "yellow"))))
     (term-blue ((t (:foreground "blue"))))
     (term-magenta ((t (:foreground "magenta"))))
     (term-cyan ((t (:foreground "cyan"))))
     (term-white ((t (:foreground "white"))))
     (term-blackbg ((t (:background "black"))))
     (term-redbg ((t (:background "red"))))
     (term-greenbg ((t (:background "green"))))
     (term-yellowbg ((t (:background "yellow"))))
     (term-bluebg ((t (:background "blue"))))
     (term-magentabg ((t (:background "magenta"))))
     (term-cyanbg ((t (:background "cyan"))))
     (term-whitebg ((t (:background "white"))))
     (gnus-emphasis-bold ((t (:bold t))))
     (gnus-emphasis-italic ((t (:italic t))))
     (gnus-emphasis-underline ((t (:underline t))))
     (gnus-emphasis-underline-bold ((t (:bold t :underline t))))
     (gnus-emphasis-underline-italic ((t (:italic t :underline t))))
     (gnus-emphasis-bold-italic ((t (:bold t :italic t))))
     (gnus-emphasis-underline-bold-italic ((t (:bold t :italic t :underline t))))
     (gnus-emphasis-highlight-words ((t (:foreground "yellow" :background "black"))))
     (gnus-signature-face ((t (:italic t))))
     (gnus-header-from-face ((t (:foreground "spring green"))))
     (gnus-header-subject-face ((t (:foreground "yellow" :bold t))))
     (gnus-header-newsgroups-face ((t (:foreground "SeaGreen3" :bold t :italic t))))
     (gnus-header-name-face ((t (:foreground "pink"))))
     (gnus-header-content-face ((t (:foreground "lime green" :italic t))))
     (gnus-cite-attribution-face ((t (:italic t))))
     (gnus-cite-face-1 ((t (:foreground "light blue"))))
     (gnus-cite-face-2 ((t (:foreground "light cyan"))))
     (gnus-cite-face-3 ((t (:foreground "light yellow"))))
     (gnus-cite-face-4 ((t (:foreground "light pink"))))
     (gnus-cite-face-5 ((t (:foreground "pale green"))))
     (gnus-cite-face-6 ((t (:foreground "beige"))))
     (gnus-cite-face-7 ((t (:foreground "orange"))))
     (gnus-cite-face-8 ((t (:foreground "magenta"))))
     (gnus-cite-face-9 ((t (:foreground "violet"))))
     (gnus-cite-face-10 ((t (:foreground "medium purple"))))
     (gnus-cite-face-11 ((t (:foreground "turquoise")))))))

(defun color-theme-sitaramv-nt ()
  "Black foreground on white background.
Includes faces for font-lock, widget, custom, speedbar."
  (interactive)
  (color-theme-install
   '(color-theme-sitaramv-nt
     ((foreground-color . "black")
      (background-color . "white")
      (mouse-color . "sienna3")
      (cursor-color . "HotPink")
      (border-color . "Blue")
      (background-mode . light))
     (default ((t (nil))))
     (modeline ((t (:foreground "black" :background "gold2"))))
     (highlight ((t (:foreground "black" :background "darkseagreen2"))))
     (bold ((t (:bold t))))
     (italic ((t (:italic t))))
     (bold-italic ((t (:bold t :italic t))))
     (region ((t (:foreground "black" :background "snow3"))))
     (secondary-selection ((t (:background "paleturquoise"))))
     (underline ((t (:underline t))))
     (lazy-highlight-face ((t (:foreground "dark magenta" :bold t))))
     (font-lock-comment-face ((t (:foreground "ForestGreen" :italic t))))
     (font-lock-string-face ((t (:foreground "red"))))
     (font-lock-keyword-face ((t (:foreground "blue" :bold t))))
     (font-lock-builtin-face ((t (:foreground "black"))))
     (font-lock-function-name-face ((t (:foreground "dark magenta" :bold t))))
     (font-lock-variable-name-face ((t (:foreground "black"))))
     (font-lock-type-face ((t (:foreground "blue"))))
     (font-lock-constant-face ((t (:foreground "CadetBlue"))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     (widget-documentation-face ((t (:foreground "dark green"))))
     (widget-button-face ((t (:bold t))))
     (widget-field-face ((t (:background "gray85"))))
     (widget-single-line-field-face ((t (:background "gray85"))))
     (widget-inactive-face ((t (:foreground "dim gray"))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))
     (custom-rogue-face ((t (:foreground "pink" :background "black"))))
     (custom-modified-face ((t (:foreground "white" :background "blue"))))
     (custom-set-face ((t (:foreground "blue" :background "white"))))
     (custom-changed-face ((t (:foreground "white" :background "blue"))))
     (custom-saved-face ((t (:underline t))))
     (custom-button-face ((t (nil))))
     (custom-documentation-face ((t (nil))))
     (custom-state-face ((t (:foreground "dark green"))))
     (custom-variable-tag-face ((t (:foreground "blue" :underline t))))
     (custom-variable-button-face ((t (:bold t :underline t))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face-1 ((t (:foreground "red" :underline t))))
     (custom-group-tag-face ((t (:foreground "blue" :underline t))))
     (speedbar-button-face ((t (:foreground "green4"))))
     (speedbar-file-face ((t (:foreground "cyan4"))))
     (speedbar-directory-face ((t (:foreground "blue4"))))
     (speedbar-tag-face ((t (:foreground "brown"))))
     (speedbar-selected-face ((t (:foreground "red" :underline t))))
     (speedbar-highlight-face ((t (:background "green"))))
     (ff-paths-non-existant-file-face ((t (:foreground "NavyBlue" :bold t))))
     (show-paren-match-face ((t (:background "light blue"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "purple")))))))

(defun color-theme-billw ()
  "Cornsilk on black.
Includes info, diary, font-lock, eshell, sgml, message, gnus,
widget, custom, latex, ediff."
  (interactive)
  (color-theme-install
   '(color-theme-billw
     ((foreground-color . "cornsilk")
      (background-color . "black")
      (mouse-color . "black")
      (cursor-color . "white")
      (border-color . "black")
      (background-mode . dark))
     (default ((t (nil))))
     (modeline ((t (:foreground "black" :background "wheat"))))
     (highlight ((t (:foreground "wheat" :background "darkslategray"))))
     (bold ((t (:bold t))))
     (italic ((t (:italic t))))
     (bold-italic ((t (:bold t :italic t))))
     (region ((t (:background "dimgray"))))
     (secondary-selection ((t (:background "deepskyblue4"))))
     (underline ((t (:underline t))))
     (info-node ((t (:foreground "yellow" :bold t :italic t))))
     (info-menu-5 ((t (:underline t))))
     (info-xref ((t (:foreground "yellow" :bold t))))
     (diary-face ((t (:foreground "orange"))))
     (calendar-today-face ((t (:underline t))))
     (holiday-face ((t (:background "red"))))
     (show-paren-match-face ((t (:background "deepskyblue4"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "red"))))
     (font-lock-comment-face ((t (:foreground "gold"))))
     (font-lock-string-face ((t (:foreground "orange"))))
     (font-lock-keyword-face ((t (:foreground "cyan1"))))
     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-function-name-face ((t (:foreground "mediumspringgreen"))))
     (font-lock-variable-name-face ((t (:foreground "light salmon"))))
     (font-lock-type-face ((t (:foreground "yellow1"))))
     (font-lock-constant-face ((t (:foreground "salmon"))))
     (font-lock-warning-face ((t (:foreground "gold" :bold t))))
     (blank-space-face ((t (:background "LightGray"))))
     (blank-tab-face ((t (:foreground "black" :background "cornsilk"))))
     (highline-face ((t (:background "gray35"))))
     (eshell-ls-directory-face ((t (:foreground "green" :bold t))))
     (eshell-ls-symlink-face ((t (:foreground "Cyan" :bold t))))
     (eshell-ls-executable-face ((t (:foreground "orange" :bold t))))
     (eshell-ls-readonly-face ((t (:foreground "gray"))))
     (eshell-ls-unreadable-face ((t (:foreground "DarkGrey"))))
     (eshell-ls-special-face ((t (:foreground "Magenta" :bold t))))
     (eshell-ls-missing-face ((t (:foreground "Red" :bold t))))
     (eshell-ls-archive-face ((t (:foreground "Orchid" :bold t))))
     (eshell-ls-backup-face ((t (:foreground "LightSalmon"))))
     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))
     (eshell-ls-clutter-face ((t (:foreground "blue" :bold t))))
     (sgml-start-tag-face ((t (:foreground "mediumspringgreen"))))
     (custom-button-face ((t (:foreground "white"))))
     (sgml-ignored-face ((t (:foreground "gray20" :background "gray60"))))
     (sgml-doctype-face ((t (:foreground "orange"))))
     (sgml-sgml-face ((t (:foreground "yellow"))))
     (vc-annotate-face-0046FF ((t (:foreground "wheat" :background "black"))))
     (custom-documentation-face ((t (:foreground "white"))))
     (sgml-end-tag-face ((t (:foreground "greenyellow"))))
     (linemenu-face ((t (:background "gray30"))))
     (sgml-entity-face ((t (:foreground "gold"))))
     (message-header-to-face ((t (:foreground "floral white" :bold t))))
     (message-header-cc-face ((t (:foreground "ivory"))))
     (message-header-subject-face ((t (:foreground "papaya whip" :bold t))))
     (message-header-newsgroups-face ((t (:foreground "lavender blush" :bold t :italic t))))
     (message-header-other-face ((t (:foreground "pale turquoise"))))
     (message-header-name-face ((t (:foreground "light sky blue"))))
     (message-header-xheader-face ((t (:foreground "blue"))))
     (message-separator-face ((t (:foreground "sandy brown"))))
     (message-cited-text-face ((t (:foreground "plum1"))))
     (message-mml-face ((t (:foreground "ForestGreen"))))
     (gnus-group-news-1-face ((t (:foreground "white" :bold t))))
     (gnus-group-news-1-empty-face ((t (:foreground "white"))))
     (gnus-group-news-2-face ((t (:foreground "lightcyan" :bold t))))
     (gnus-group-news-2-empty-face ((t (:foreground "lightcyan"))))
     (gnus-group-news-3-face ((t (:foreground "tan" :bold t))))
     (gnus-group-news-3-empty-face ((t (:foreground "tan"))))
     (gnus-group-news-4-face ((t (:foreground "white" :bold t))))
     (gnus-group-news-4-empty-face ((t (:foreground "white"))))
     (gnus-group-news-5-face ((t (:foreground "wheat" :bold t))))
     (gnus-group-news-5-empty-face ((t (:foreground "wheat"))))
     (gnus-group-news-6-face ((t (:foreground "tan" :bold t))))
     (gnus-group-news-6-empty-face ((t (:foreground "tan"))))
     (gnus-group-news-low-face ((t (:foreground "DarkTurquoise" :bold t))))
     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))
     (gnus-group-mail-1-face ((t (:foreground "white" :bold t))))
     (gnus-group-mail-1-empty-face ((t (:foreground "gray80"))))
     (gnus-group-mail-2-face ((t (:foreground "lightcyan" :bold t))))
     (gnus-group-mail-2-empty-face ((t (:foreground "lightcyan"))))
     (gnus-group-mail-3-face ((t (:foreground "tan" :bold t))))
     (gnus-group-mail-3-empty-face ((t (:foreground "tan"))))
     (gnus-group-mail-low-face ((t (:foreground "aquamarine4" :bold t))))
     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))
     (gnus-summary-selected-face ((t (:background "deepskyblue4" :underline t))))
     (gnus-summary-cancelled-face ((t (:foreground "black" :background "gray"))))
     (gnus-summary-high-ticked-face ((t (:foreground "gray70" :bold t))))
     (gnus-summary-low-ticked-face ((t (:foreground "gray70" :bold t))))
     (gnus-summary-normal-ticked-face ((t (:foreground "gray70" :bold t))))
     (gnus-summary-high-ancient-face ((t (:foreground "SkyBlue" :bold t))))
     (gnus-summary-low-ancient-face ((t (:foreground "SkyBlue" :italic t))))
     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))
     (gnus-summary-high-unread-face ((t (:bold t))))
     (gnus-summary-low-unread-face ((t (:italic t))))
     (gnus-summary-normal-unread-face ((t (nil))))
     (gnus-summary-high-read-face ((t (:foreground "PaleGreen" :bold t))))
     (gnus-summary-low-read-face ((t (:foreground "PaleGreen" :italic t))))
     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))
     (gnus-splash-face ((t (:foreground "gold"))))
     (font-latex-bold-face ((t (nil))))
     (font-latex-italic-face ((t (nil))))
     (font-latex-math-face ((t (nil))))
     (font-latex-sedate-face ((t (:foreground "Gray85"))))
     (font-latex-string-face ((t (:foreground "orange"))))
     (font-latex-warning-face ((t (:foreground "gold"))))
     (widget-documentation-face ((t (:foreground "lime green"))))
     (widget-button-face ((t (:bold t))))
     (widget-field-face ((t (:background "gray20"))))
     (widget-single-line-field-face ((t (:background "gray20"))))
     (widget-inactive-face ((t (:foreground "wheat"))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))
     (custom-rogue-face ((t (:foreground "pink" :background "black"))))
     (custom-modified-face ((t (:foreground "white" :background "blue"))))
     (custom-set-face ((t (:foreground "blue"))))
     (custom-changed-face ((t (:foreground "wheat" :background "skyblue"))))
     (custom-saved-face ((t (:underline t))))
     (custom-state-face ((t (:foreground "light green"))))
     (custom-variable-tag-face ((t (:foreground "skyblue" :underline t))))
     (custom-variable-button-face ((t (:bold t :underline t))))
     (custom-face-tag-face ((t (:foreground "white" :underline t))))
     (custom-group-tag-face-1 ((t (:foreground "pink" :underline t))))
     (custom-group-tag-face ((t (:foreground "skyblue" :underline t))))
     (swbuff-current-buffer-face ((t (:foreground "red" :bold t))))
     (ediff-current-diff-face-A ((t (:foreground "firebrick" :background "pale green"))))
     (ediff-current-diff-face-B ((t (:foreground "DarkOrchid" :background "Yellow"))))
     (ediff-current-diff-face-C ((t (:foreground "white" :background "indianred"))))
     (ediff-current-diff-face-Ancestor ((t (:foreground "Black" :background "VioletRed"))))
     (ediff-fine-diff-face-A ((t (:foreground "Navy" :background "sky blue"))))
     (ediff-fine-diff-face-B ((t (:foreground "Black" :background "cyan"))))
     (ediff-fine-diff-face-C ((t (:foreground "Black" :background "Turquoise"))))
     (ediff-fine-diff-face-Ancestor ((t (:foreground "Black" :background "Green"))))
     (ediff-even-diff-face-A ((t (:foreground "Black" :background "light grey"))))
     (ediff-even-diff-face-B ((t (:foreground "White" :background "Grey"))))
     (ediff-even-diff-face-C ((t (:foreground "Black" :background "light grey"))))
     (ediff-even-diff-face-Ancestor ((t (:foreground "White" :background "Grey"))))
     (ediff-odd-diff-face-A ((t (:foreground "White" :background "Grey"))))
     (ediff-odd-diff-face-B ((t (:foreground "Black" :background "light grey"))))
     (ediff-odd-diff-face-C ((t (:foreground "White" :background "Grey"))))
     (ediff-odd-diff-face-Ancestor ((t (:foreground "Black" :background "light grey"))))
     (gnus-emphasis-bold ((t (:bold t))))
     (gnus-emphasis-italic ((t (:italic t))))
     (gnus-emphasis-underline ((t (:foreground "white" :background "goldenrod4"))))
     (gnus-emphasis-underline-bold ((t (:foreground "black" :background "yellow" :bold t :underline t))))
     (gnus-emphasis-underline-italic ((t (:foreground "black" :background "yellow" :italic t :underline t))))
     (gnus-emphasis-bold-italic ((t (:bold t :italic t))))
     (gnus-emphasis-underline-bold-italic ((t (:foreground "black" :background "yellow" :bold t :italic t :underline t))))
     (gnus-emphasis-highlight-words ((t (:foreground "yellow" :background "black"))))
     (gnus-signature-face ((t (:italic t))))
     (gnus-header-from-face ((t (:foreground "wheat"))))
     (gnus-header-subject-face ((t (:foreground "wheat" :bold t))))
     (gnus-header-newsgroups-face ((t (:foreground "wheat" :italic t))))
     (gnus-header-name-face ((t (:foreground "white"))))
     (gnus-header-content-face ((t (:foreground "tan" :italic t))))
     (gnus-filterhist-face-1 ((t (nil))))
     (gnus-splash ((t (:foreground "Brown"))))
     (gnus-cite-attribution-face ((t (:italic t))))
     (gnus-cite-face-1 ((t (:foreground "light blue"))))
     (gnus-cite-face-2 ((t (:foreground "light cyan"))))
     (gnus-cite-face-3 ((t (:foreground "light yellow"))))
     (gnus-cite-face-4 ((t (:foreground "light pink"))))
     (gnus-cite-face-5 ((t (:foreground "pale green"))))
     (gnus-cite-face-6 ((t (:foreground "beige"))))
     (gnus-cite-face-7 ((t (:foreground "orange"))))
     (gnus-cite-face-8 ((t (:foreground "magenta"))))
     (gnus-cite-face-9 ((t (:foreground "violet"))))
     (gnus-cite-face-10 ((t (:foreground "medium purple"))))
     (gnus-cite-face-11 ((t (:foreground "turquoise")))))))

(defun color-theme-retro-green (&optional color func)
  "Plain green on black faces for those longing for the good old days."
  (interactive)
  ;; Build a list of faces without parameters
  (let ((old-faces (face-list))
	(faces)
	(face)
	(foreground (or color "green")))
    (while old-faces
      (setq face (car old-faces)
	    old-faces (cdr old-faces))
      (cond ((memq face '(bold bold-italic))
	     (add-to-list 'faces `(,face (( t (:bold t))))))
	    ((memq face '(italic underline show-paren-mismatch-face))
	     (add-to-list 'faces `(,face (( t (:underline t))))))
	    ((memq face '(modeline highlight region secondary-selection 
				   show-paren-match-face))
	     (add-to-list 'faces `(,face (( t (:foreground "black" 
					       :background ,foreground 
					       :inverse t))))))
	    (t
	     (add-to-list 'faces `(,face (( t (nil))))))))
    (color-theme-install
     (append
      (list (or func 'color-theme-retro-green)
	    (list (cons 'foreground-color foreground)
		  (cons 'background-color "black")
		  (cons 'mouse-color foreground)
		  (cons 'cursor-color foreground)
		  (cons 'border-color foreground)
		  (cons 'background-mode 'dark)))
      faces))))

(defun color-theme-retro-orange ()
  "Plain orange on black faces for those longing for the good old days."
  (interactive)
  (color-theme-retro-green "orange" 'color-theme-retro-orange))

(defun color-theme-subtle-hacker ()
  "Subtle Hacker Color Theme.
Based on gnome2, but uses white for important things like comments,
and less of the unreadable tomato.  By Colin Walters <levanti@verbum.org>"
  (interactive)
  (color-theme-gnome2)
  (color-theme-install
   '(color-theme-subtle-hacker
     nil
     nil
    (custom-state-face ((t (:foreground "Coral"))))
    (diary-face ((t (:bold t :foreground "IndianRed"))))
    (eshell-ls-clutter-face ((t (:bold t :foreground "DimGray"))))
    (eshell-ls-executable-face ((t (:bold t :foreground "Coral"))))
    (eshell-ls-missing-face ((t (:bold t :foreground "black"))))
    (eshell-ls-special-face ((t (:bold t :foreground "Gold"))))
    (eshell-ls-symlink-face ((t (:bold t :foreground "White"))))
    (font-lock-comment-face ((t (:foreground "White"))))
    (font-lock-constant-face ((t (:bold t :foreground "Aquamarine"))))
    (font-lock-function-name-face ((t (:bold t :foreground "MediumSlateBlue"))))
    (font-lock-string-face ((t (:italic t :foreground "LightSalmon"))))
    (font-lock-variable-name-face ((t (:italic t :bold t :foreground "Aquamarine"))))
    (gnus-cite-face-1 ((t (:foreground "dark khaki"))))
    (gnus-cite-face-2 ((t (:foreground "chocolate"))))
    (gnus-cite-face-3 ((t (:foreground "tomato"))))
    (gnus-group-mail-1-empty-face ((t (:foreground "light cyan"))))
    (gnus-group-mail-1-face ((t (:bold t :foreground "light cyan"))))
    (gnus-group-mail-2-empty-face ((t (:foreground "turquoise"))))
    (gnus-group-mail-2-face ((t (:bold t :foreground "turquoise"))))
    (gnus-group-mail-3-empty-face ((t (:foreground "tomato"))))
    (gnus-group-mail-3-face ((t (:bold t :foreground "tomato"))))
    (gnus-group-mail-low-empty-face ((t (:foreground "dodger blue"))))
    (gnus-group-mail-low-face ((t (:bold t :foreground "dodger blue"))))
    (gnus-group-news-1-empty-face ((t (:foreground "green yellow"))))
    (gnus-group-news-1-face ((t (:bold t :foreground "green yellow"))))
    (gnus-group-news-2-empty-face ((t (:foreground "dark orange"))))
    (gnus-group-news-2-face ((t (:bold t :foreground "dark orange"))))
    (gnus-group-news-3-empty-face ((t (:foreground "tomato"))))
    (gnus-group-news-3-face ((t (:bold t :foreground "tomato"))))
    (gnus-group-news-low-empty-face ((t (:foreground "yellow green"))))
    (gnus-group-news-low-face ((t (:bold t :foreground "yellow green"))))
    (gnus-header-name-face ((t (:bold t :foreground "DodgerBlue1"))))
    (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3"))))
    (gnus-signature-face ((t (:foreground "salmon"))))
    (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
    (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))
    (gnus-summary-high-read-face ((t (:bold t :foreground "forest green"))))
    (gnus-summary-high-ticked-face ((t (:bold t :foreground "burlywood"))))
    (gnus-summary-high-unread-face ((t (:italic t :bold t :foreground "cyan"))))
    (gnus-summary-low-ancient-face ((t (:italic t :foreground "chocolate"))))
    (gnus-summary-low-read-face ((t (:foreground "light sea green"))))
    (gnus-summary-low-ticked-face ((t (:italic t :foreground "chocolate"))))
    (gnus-summary-low-unread-face ((t (:italic t :foreground "light sea green"))))
    (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))
    (gnus-summary-normal-read-face ((t (:foreground "khaki"))))
    (gnus-summary-normal-ticked-face ((t (:foreground "sandy brown"))))
    (gnus-summary-normal-unread-face ((t (:foreground "aquamarine"))))
    (message-cited-text-face ((t (:foreground "White"))))
    (message-header-name-face ((t (:foreground "DodgerBlue1"))))
    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3"))))
    (message-header-other-face ((t (:foreground "LightSkyBlue3"))))
    (message-header-xheader-face ((t (:foreground "DodgerBlue3")))))))

(defun color-theme-pok-wog ()
  "Low-contrast White-on-Gray by S.Pokrovsky.

The following might be a good addition to your .Xdefaults file:

Emacs.pane.menubar.background: darkGrey
Emacs.pane.menubar.foreground: black"
  (interactive)
  (color-theme-install
   '(color-theme-pok-wog
     ((foreground-color . "dark gray")
      (background-color . "DarkSlateGray")
      (mouse-color . "gold")
      (cursor-color . "Cyan")
      (border-color . "black")
      (background-mode . dark))
     (bold ((t (:bold t :foreground "Wheat"))))
     (bold-italic ((t (:italic t :bold t :foreground "wheat"))))
     (calendar-today-face ((t (:underline t :foreground "white"))))
     (default ((t (:background "DarkSlateGray" :foreground "White"))))
   (diary-face ((t (:foreground "red"))))
   (font-lock-builtin-face ((t (:bold t :foreground "cyan"))))
   (font-lock-comment-face ((t (:foreground "Gold"))))
   (font-lock-constant-face ((t (:bold t :foreground "LightSteelBlue"))))
   (font-lock-function-name-face ((t (:bold t :foreground "Yellow"))))
   (font-lock-keyword-face ((t (:bold t :foreground "Cyan"))))
   (font-lock-string-face ((t (:foreground "Khaki"))))
   (font-lock-type-face ((t (:bold t :foreground "Cyan"))))
   (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
   (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
   (gnus-cite-attribution-face ((t (:bold t :foreground "Wheat"))))
   (gnus-cite-face-1 ((t (:foreground "wheat"))))
   (gnus-cite-face-10 ((t (:foreground "wheat"))))
   (gnus-cite-face-11 ((t (:foreground "turquoise"))))
   (gnus-cite-face-2 ((t (:foreground "cyan"))))
   (gnus-cite-face-3 ((t (:foreground "light yellow"))))
   (gnus-cite-face-4 ((t (:foreground "light pink"))))
   (gnus-cite-face-5 ((t (:foreground "pale green"))))
   (gnus-cite-face-6 ((t (:foreground "beige"))))
   (gnus-cite-face-7 ((t (:foreground "orange"))))
   (gnus-cite-face-8 ((t (:foreground "magenta"))))
   (gnus-cite-face-9 ((t (:foreground "violet"))))
   (gnus-emphasis-bold ((t (:bold t :foreground "wheat"))))
   (gnus-emphasis-bold-italic ((t (:italic t :bold t))))
   (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))
   (gnus-emphasis-italic ((t (:italic t :foreground "white"))))
   (gnus-emphasis-underline ((t (:underline t :foreground "white"))))
   (gnus-emphasis-underline-bold ((t (:underline t :bold t :foreground "wheat"))))
   (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))
   (gnus-emphasis-underline-italic ((t (:underline t :italic t :foreground "white"))))
   (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))
   (gnus-group-mail-1-face ((t (:bold t :foreground "aquamarine1"))))
   (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))
   (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2"))))
   (gnus-group-mail-3-empty-face ((t (:foreground "Salmon"))))
   (gnus-group-mail-3-face ((t (:bold t :foreground "gold"))))
   (gnus-group-mail-low-empty-face ((t (:foreground "Wheat"))))
   (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4"))))
   (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))
   (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise"))))
   (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))
   (gnus-group-news-2-face ((t (:bold t :foreground "turquoise"))))
   (gnus-group-news-3-empty-face ((t (nil))))
   (gnus-group-news-3-face ((t (:bold t :foreground "Wheat"))))
   (gnus-group-news-4-empty-face ((t (nil))))
   (gnus-group-news-4-face ((t (:bold t))))
   (gnus-group-news-5-empty-face ((t (nil))))
   (gnus-group-news-5-face ((t (:bold t))))
   (gnus-group-news-6-empty-face ((t (nil))))
   (gnus-group-news-6-face ((t (:bold t))))
   (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))
   (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise"))))
   (gnus-header-content-face ((t (:italic t :foreground "Wheat"))))
   (gnus-header-from-face ((t (:foreground "light yellow"))))
   (gnus-header-name-face ((t (:foreground "cyan"))))
   (gnus-header-newsgroups-face ((t (:italic t :foreground "yellow"))))
   (gnus-header-subject-face ((t (:bold t :foreground "Gold"))))
   (gnus-signature-face ((t (:italic t :foreground "wheat"))))
   (gnus-splash-face ((t (:foreground "orange"))))
   (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
   (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue"))))
   (gnus-summary-high-read-face ((t (:bold t :foreground "PaleGreen"))))
   (gnus-summary-high-ticked-face ((t (:bold t :foreground "pink"))))
   (gnus-summary-high-unread-face ((t (:bold t :foreground "gold"))))
   (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue"))))
   (gnus-summary-low-read-face ((t (:italic t :foreground "PaleGreen"))))
   (gnus-summary-low-ticked-face ((t (:italic t :foreground "pink"))))
   (gnus-summary-low-unread-face ((t (:italic t))))
   (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))
   (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))
   (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))
   (gnus-summary-normal-unread-face ((t (:foreground "wheat"))))
   (gnus-summary-selected-face ((t (:underline t :foreground "white"))))
   (highlight ((t (:background "Blue" :foreground "white"))))
   (highline-face ((t (:background "black" :foreground "white"))))
   (holiday-face ((t (:background "pink" :foreground "white"))))
   (info-menu-5 ((t (:underline t))))
   (info-node ((t (:italic t :bold t :foreground "white"))))
   (info-xref ((t (:bold t :foreground "wheat"))))
   (italic ((t (:italic t :foreground "white"))))
   (makefile-space-face ((t (:background "hotpink"))))
   (message-cited-text-face ((t (:foreground "green"))))
   (message-header-cc-face ((t (:bold t :foreground "Aquamarine"))))
   (message-header-name-face ((t (:foreground "Gold"))))
   (message-header-newsgroups-face ((t (:italic t :bold t :foreground "yellow"))))
   (message-header-other-face ((t (:foreground "lightGray"))))
   (message-header-subject-face ((t (:foreground "Yellow"))))
   (message-header-to-face ((t (:bold t :foreground "green2"))))
   (message-header-xheader-face ((t (:foreground "blue"))))
   (message-mml-face ((t (:bold t :foreground "khaki"))))
   (message-separator-face ((t (:background "aquamarine" :foreground "black"))))
   (modeline ((t (:background "DarkGray" :foreground "Black"))))
   (paren-mismatch-face ((t (:background "DeepPink" :foreground "white"))))
   (paren-no-match-face ((t (:background "yellow" :foreground "white"))))
   (region ((t (:background "MediumSlateBlue" :foreground "white"))))
   (secondary-selection ((t (:background "Sienna" :foreground "white"))))
   (show-paren-match-face ((t (:background "turquoise" :foreground "white"))))
   (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))
   (speedbar-button-face ((t (:bold t :foreground "magenta"))))
   (speedbar-directory-face ((t (:bold t :foreground "orchid"))))
   (speedbar-file-face ((t (:foreground "pink"))))
   (speedbar-highlight-face ((t (:background "black"))))
   (speedbar-selected-face ((t (:underline t :foreground "cyan"))))
   (speedbar-tag-face ((t (:foreground "yellow"))))
   (swbuff-current-buffer-face ((t (:bold t :foreground "red"))))
   (underline ((t (:underline t :foreground "white"))))
   (widget-button-face ((t (:bold t :foreground "wheat"))))
   (widget-button-pressed-face ((t (:foreground "red"))))
   (widget-documentation-face ((t (:foreground "lime green"))))
   (widget-field-face ((t (:background "dim gray" :foreground "white"))))
   (widget-inactive-face ((t (:foreground "light gray"))))
   (widget-single-line-field-face ((t (:background "dim gray" :foreground "white"))))
)))

(defun color-theme-pok-wob ()
  "White-on-Black by S. Pokrovsky.

The following might be a good addition to your .Xdefaults file:

Emacs.pane.menubar.background: darkGrey
Emacs.pane.menubar.foreground: black"
  (interactive)
;  (setq term-default-fg-color "white"
;        term-default-bg "black")
  (color-theme-install
   '(color-theme-pok-wob
     ((foreground-color . "dark gray")
      (background-color . "black")
      (mouse-color . "gold")
      (cursor-color . "yellow")
      (border-color . "black")
      (background-mode . dark))
   (bold ((t (:bold t :foreground "light gray"))))
   (bold-italic ((t (:italic t :bold t :foreground "cyan"))))
   (calendar-today-face ((t (:underline t :foreground "white"))))
   (custom-button-face ((t (nil))))
   (custom-changed-face ((t (:background "blue" :foreground "white"))))
   (custom-documentation-face ((t (nil))))
   (custom-face-tag-face ((t (:underline t))))
   (custom-group-tag-face ((t (:underline t))))
   (custom-group-tag-face-1 ((t (:underline t))))
   (custom-invalid-face ((t (:background "red" :foreground "white"))))
   (custom-modified-face ((t (:background "blue" :foreground "white"))))
   (custom-rogue-face ((t (:background "black" :foreground "pink"))))
   (custom-saved-face ((t (:underline t))))
   (custom-set-face ((t (:background "white" :foreground "blue"))))
   (custom-state-face ((t (nil))))
   (custom-variable-button-face ((t (:underline t :bold t))))
   (custom-variable-tag-face ((t (:underline t))))
   (default ((t (:background "black" :foreground "white"))))
   (diary-face ((t (:foreground "gold"))))
   (font-lock-builtin-face ((t (:bold t :foreground "cyan"))))
   (font-lock-comment-face ((t (:foreground "Gold"))))
   (font-lock-constant-face ((t (:bold t :foreground "LightSteelBlue"))))
   (font-lock-function-name-face ((t (:bold t :foreground "gold"))))
   (font-lock-keyword-face ((t (:bold t :foreground "Cyan"))))
   (font-lock-string-face ((t (:foreground "Khaki"))))
   (font-lock-type-face ((t (:bold t :foreground "Cyan"))))
   (font-lock-variable-name-face ((t (:italic t :foreground "gold"))))
   (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
   (gnus-cite-attribution-face ((t (:underline t :foreground "beige"))))
   (gnus-cite-face-1 ((t (:foreground "gold"))))
   (gnus-cite-face-10 ((t (:foreground "coral"))))
   (gnus-cite-face-11 ((t (:foreground "turquoise"))))
   (gnus-cite-face-2 ((t (:foreground "wheat"))))
   (gnus-cite-face-3 ((t (:foreground "light pink"))))
   (gnus-cite-face-4 ((t (:foreground "khaki"))))
   (gnus-cite-face-5 ((t (:foreground "pale green"))))
   (gnus-cite-face-6 ((t (:foreground "beige"))))
   (gnus-cite-face-7 ((t (:foreground "orange"))))
   (gnus-cite-face-8 ((t (:foreground "magenta"))))
   (gnus-cite-face-9 ((t (:foreground "violet"))))
   (gnus-emphasis-bold ((t (:bold t :foreground "light gray"))))
   (gnus-emphasis-bold-italic ((t (:italic t :bold t :foreground "cyan"))))
   (gnus-emphasis-highlight-words ((t (:background "black" :foreground "gold"))))
   (gnus-emphasis-italic ((t (:italic t :foreground "cyan"))))
   (gnus-emphasis-underline ((t (:underline t :foreground "white"))))
   (gnus-emphasis-underline-bold ((t (:underline t :bold t :foreground "white"))))
   (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t :foreground "white"))))
   (gnus-emphasis-underline-italic ((t (:underline t :italic t :foreground "white"))))
   (gnus-group-mail-1-empty-face ((t (:foreground "Magenta"))))
   (gnus-group-mail-1-face ((t (:bold t :foreground "Magenta"))))
   (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))
   (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2"))))
   (gnus-group-mail-3-empty-face ((t (:foreground "Cyan"))))
   (gnus-group-mail-3-face ((t (:bold t :foreground "Cyan"))))
   (gnus-group-mail-low-empty-face ((t (:foreground "Wheat"))))
   (gnus-group-mail-low-face ((t (:foreground "aquamarine4"))))
   (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))
   (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise"))))
   (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))
   (gnus-group-news-2-face ((t (:bold t :foreground "turquoise"))))
   (gnus-group-news-3-empty-face ((t (:foreground "wheat"))))
   (gnus-group-news-3-face ((t (:bold t :foreground "Wheat"))))
   (gnus-group-news-4-empty-face ((t (nil))))
   (gnus-group-news-4-face ((t (:bold t))))
   (gnus-group-news-5-empty-face ((t (nil))))
   (gnus-group-news-5-face ((t (:bold t))))
   (gnus-group-news-6-empty-face ((t (nil))))
   (gnus-group-news-6-face ((t (:bold t))))
   (gnus-group-news-low-empty-face ((t (:foreground "MediumAquamarine"))))
   (gnus-group-news-low-face ((t (:bold t :foreground "MediumAquamarine"))))
   (gnus-header-content-face ((t (:italic t :foreground "Wheat"))))
   (gnus-header-from-face ((t (:foreground "light yellow"))))
   (gnus-header-name-face ((t (:foreground "Wheat"))))
   (gnus-header-newsgroups-face ((t (:italic t :foreground "gold"))))
   (gnus-header-subject-face ((t (:bold t :foreground "Gold"))))
   (gnus-signature-face ((t (:italic t :foreground "white"))))
   (gnus-splash-face ((t (:foreground "orange"))))
   (gnus-summary-cancelled-face ((t (:background "black" :foreground "orange"))))
   (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue"))))
   (gnus-summary-high-read-face ((t (:bold t :foreground "red"))))
   (gnus-summary-high-ticked-face ((t (:bold t :foreground "coral"))))
   (gnus-summary-high-unread-face ((t (:bold t :foreground "gold"))))
   (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue"))))
   (gnus-summary-low-read-face ((t (:italic t :foreground "red"))))
   (gnus-summary-low-ticked-face ((t (:italic t :foreground "coral"))))
   (gnus-summary-low-unread-face ((t (:italic t :foreground "white"))))
   (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))
   (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))
   (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))
   (gnus-summary-normal-unread-face ((t (:foreground "white"))))
   (gnus-summary-selected-face ((t (:underline t :foreground "white"))))
   (highlight ((t (:background "Blue" :foreground "white"))))
   (highline-face ((t (:background "dark slate gray" :foreground "white"))))
   (holiday-face ((t (:background "red" :foreground "white"))))
   (info-menu-5 ((t (:underline t))))
   (info-node ((t (:italic t :bold t :foreground "white"))))
   (info-xref ((t (:bold t :foreground "light gray"))))
   (italic ((t (:italic t :foreground "cyan"))))
   (makefile-space-face ((t (:background "hotpink" :foreground "white"))))
   (message-cited-text-face ((t (:foreground "green"))))
   (message-header-cc-face ((t (:bold t :foreground "Aquamarine"))))
   (message-header-name-face ((t (:foreground "Gold"))))
   (message-header-newsgroups-face ((t (:italic t :bold t :foreground "gold"))))
   (message-header-other-face ((t (:foreground "lightGray"))))
   (message-header-subject-face ((t (:foreground "Yellow"))))
   (message-header-to-face ((t (:bold t :foreground "green2"))))
   (message-header-xheader-face ((t (:foreground "sky blue"))))
   (message-mml-face ((t (:bold t :foreground "khaki"))))
   (message-separator-face ((t (:background "aquamarine" :foreground "black"))))
   (modeline ((t (:background "dark gray" :foreground "black"))))
   (paren-mismatch-face ((t (:bold t :background "white" :foreground "red"))))
   (paren-no-match-face ((t (:bold t :background "white" :foreground "red"))))
   (region ((t (:background "MediumSlateBlue" :foreground "white"))))
   (secondary-selection ((t (:background "Sienna" :foreground "white"))))
   (show-paren-match-face ((t (:background "purple" :foreground "white"))))
   (show-paren-mismatch-face ((t (:bold t :background "white" :foreground "red"))))
   (speedbar-button-face ((t (nil))))
   (speedbar-directory-face ((t (nil))))
   (speedbar-file-face ((t (:bold t))))
   (speedbar-highlight-face ((t (nil))))
   (speedbar-selected-face ((t (:underline t))))
   (speedbar-tag-face ((t (nil))))
   (swbuff-current-buffer-face ((t (:bold t :foreground "red"))))
;   (term-black ((t (:background "black" :foreground "black"))))
;   (term-blackbg ((t (:background "black" :foreground "white"))))
;   (term-blue ((t (:background "black" :foreground "blue"))))
;   (term-bluebg ((t (:background "blue" :foreground "white"))))
;   (term-bold ((t (:bold t :background "black" :foreground "white"))))
;   (term-cyan ((t (:background "black" :foreground "cyan"))))
;   (term-cyanbg ((t (:background "cyan" :foreground "white"))))
;   (term-default-bg ((t (:foreground "white"))))
;   (term-default-bg-inv ((t (:background "white" :foreground "white"))))
;   (term-default-fg ((t (:background "black" :foreground "white"))))
;   (term-default-fg-inv ((t (:background "black"))))
;   (term-green ((t (:background "black" :foreground "green"))))
;   (term-greenbg ((t (:background "green" :foreground "white"))))
;   (term-invisible ((t (:foreground "white"))))
;   (term-invisible-inv ((t (:background "white" :foreground "white"))))
;   (term-magenta ((t (:background "black" :foreground "magenta"))))
;   (term-magentabg ((t (:background "magenta" :foreground "white"))))
;   (term-red ((t (:background "black" :foreground "red"))))
;   (term-redbg ((t (:background "red" :foreground "white"))))
;   (term-underline ((t (:underline t :background "black" :foreground "white"))))
;   (term-white ((t (:background "black" :foreground "white"))))
;   (term-whitebg ((t (:background "white" :foreground "white"))))
;   (term-yellow ((t (:background "black" :foreground "yellow"))))
;   (term-yellowbg ((t (:background "yellow" :foreground "white"))))
   (underline ((t (:underline t :foreground "white"))))
   (widget-button-face ((t (:bold t :foreground "coral"))))
   (widget-button-pressed-face ((t (:foreground "red"))))
   (widget-documentation-face ((t (:foreground "lime green"))))
   (widget-field-face ((t (:background "dim gray" :foreground "white"))))
   (widget-inactive-face ((t (:foreground "light gray"))))
   (widget-single-line-field-face ((t (:background "dim gray" :foreground "white"))))
)))

(provide 'color-theme)

;;; color-theme.el ends here
