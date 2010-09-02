;; using the Common Lisp (CL) package
(require 'cl)

;; matches substrings as u switches buffers
(load "iswitchb")
(iswitchb-default-keybindings)

;; To use Desktop
(desktop-load-default)
(desktop-read)

(when (or running-emacs19 running-emacs2031 running-emacs2061 running-emacs2041)
  ;; automatically unzips, untars files  
  (require 'uncompress)
  (load "ssh")
  (load "count-words")
)

;; Load stuff explicitly for XEmacs/Lucid Emacs
(when (or running-xemacs20 running-xemacs204 running-xemacs21R running-xemacs19)
  (require 'crypt)
  (require 'scroll-in-place)
  (require 'filladapt)
  (add-hook 'text-mode-hook 'turn-on-filladapt-mode)
  (add-hook 'c-mode-hook 'turn-off-filladapt-mode)
  (require 'crypt)
)
