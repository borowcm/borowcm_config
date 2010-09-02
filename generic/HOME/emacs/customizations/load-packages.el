;; using the Common Lisp (CL) package
(require 'cl)

;; matches substrings as u switches buffers
(require 'iswitchb)
(iswitchb-default-keybindings)
(add-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup)

;; like vi's :set nu -> numbers all lines on left-hand side
(require 'setnu)

(when (or running-emacs2071)
  ;; automatically unzips, untars files  
  (require 'uncompress)
  (require 'ssh)
  (require 'count-words)
)

;; Load stuff explicitly for XEmacs/Lucid Emacs
(when (or running-xemacs211)
  (require 'crypt)
  (require 'scroll-in-place)
  (require 'filladapt)
  (add-hook 'text-mode-hook 'turn-on-filladapt-mode)
  (add-hook 'c-mode-hook 'turn-off-filladapt-mode)
)

;; For ANTLR
(autoload 'antlr-mode "antlr-mode" nil t)
(setq auto-mode-alist (cons '("\\.g\\'" . antlr-mode) auto-mode-alist))
(add-hook 'speedbar-load-hook  ; would be too late in antlr-mode.el
	  (lambda () (speedbar-add-supported-extension ".g")))


;; for doxygen : C++ documentation generator
(require 'doxygen)
(autoload 'basis-insert-std-copyright "basis-utils"
  "Insert the standard basis copyright." t)

(autoload 'basis-insert-include-file "basis-utils"
  "Insert a guarded include file." t)

(autoload 'basis-insert-include-file-guards "basis-utils"
  "Insert guards around an include file." t)

(autoload 'basis-insert-include-file-guards-in-region "basis-utils"
  "Insert guards around all include defs within a region." t)

;; For Eiffel
;;(add-to-list 'auto-mode-alist '("\\.e\\'" . eiffel-mode))               
;;(autoload 'eiffel-mode "eiffel-mode" "Major mode for Eiffel programs" t)

(require 'autorevert)

(require 'sblox-mode)
