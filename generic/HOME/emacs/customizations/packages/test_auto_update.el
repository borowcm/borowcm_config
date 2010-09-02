(setq load-path (append load-path
                        (list nil 
			      (expand-file-name "/home/rradhakr/emacs")
			      (expand-file-name "/home/rradhakr/emacs/customizations")
			      (expand-file-name "/home/rradhakr/emacs/customizations/packages")
			      )))

(require 'auto_update)

(auto_update_design "/home/rradhakr/research/synthesis/swift/user_interface/src/basic.script" )
